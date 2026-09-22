# Tests for reading and writing FSL matrices, and for converting transformations between voxel and world space.

test_that("An FSL matrix file can be read.", {
  mat <- matrix(c(1, 0, 0, 3, 0, 1, 0, -2, 0, 0, 1, 4, 0, 0, 0, 1), ncol = 4L, byrow = TRUE)
  mat_file <- tempfile(fileext = ".mat")
  writeLines(apply(mat, 1L, function(r) paste(sprintf("%.12g", r), collapse = " ")), mat_file)

  tf <- read.fs.transform(mat_file)
  expect_true(is.fs.transform(tf))
  expect_equal(tf$matrix, mat)
  expect_equal(tf$format, "fslmat")
  expect_equal(tf$space_in, "voxel")
  expect_equal(tf$space_out, "voxel")
  expect_equal(tf$voxel_base, 0L) # FSL voxel indices are zero-based
  expect_true(is.null(tf$src))
  expect_true(is.null(tf$dst))
  expect_equal(tf$source, mat_file)
  expect_equal(read.fs.transform(mat_file, format = "fslmat")$matrix, mat)

  # Comments, blank lines and extra whitespace are tolerated.
  writeLines(c("# an FSL matrix", "", "  1 0 0 3  ", "0 1 0 -2", "0 0 1 4", "0 0 0 1", ""), mat_file)
  expect_equal(read.fs.transform(mat_file)$matrix, mat)

  # Files that are not FSL matrices.
  writeLines(c("1 0 0", "0 1 0"), mat_file)
  expect_error(read.fs.transform(mat_file))
  writeLines(c("a b c d", "0 1 0 0", "0 0 1 0", "0 0 0 1"), mat_file)
  expect_error(read.fs.transform(mat_file, format = "fslmat"))
  expect_error(read.fs.transform(file.path(tempdir(), "does_not_exist.mat")))
  unlink(mat_file)
})


test_that("The format of a transformation file is determined from its content and its extension.", {
  mat <- diag(4L)
  mat[1L, 4L] <- 5.0
  mat_file <- tempfile(fileext = ".mat")
  writeLines(apply(mat, 1L, function(r) paste(sprintf("%.12g", r), collapse = " ")), mat_file)
  expect_equal(guess.transform.format(mat_file), "fslmat")

  # A file without a known extension is identified by its content.
  no_extension <- tempfile()
  writeLines(apply(mat, 1L, function(r) paste(sprintf("%.12g", r), collapse = " ")), no_extension)
  expect_equal(guess.transform.format(no_extension), "fslmat")

  # ITK/ANTs text transforms are identified as the ITK format, which the reader can handle.
  itk_file <- tempfile(fileext = ".tfm")
  writeLines(c(
    "#Insight Transform File V1.0", "#Transform 0", "Transform: AffineTransform_float_3_3",
    "Parameters: 1 0 0 0 1 0 0 0 1 0 0 0", "FixedParameters: 0 0 0"
  ), itk_file)
  expect_equal(guess.transform.format(itk_file), "itk")

  # Binary ITK/ANTs transforms share the '.mat' extension with FSL matrices. They are identified as ITK as well,
  # and the reader reports that the binary variant is not supported instead of parsing garbage.
  itk_mat <- tempfile(fileext = ".mat")
  writeBin(c(as.raw(0L), charToRaw("AffineTransform_double_3_3"), as.raw(0L), charToRaw("fixed")), itk_mat)
  expect_equal(guess.transform.format(itk_mat), "itk")
  expect_error(read.fs.transform(itk_mat), "binary ITK or ANTs transform")

  # The FreeSurfer formats are identified by their extension.
  expect_equal(guess.transform.format(system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)), "lta")
  expect_equal(guess.transform.format(system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)), "xfm")
  expect_equal(guess.transform.format(system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE)), "dat")

  # Content that cannot be identified at all.
  unknown_file <- tempfile(fileext = ".unknown")
  writeLines(c("hello", "world"), unknown_file)
  expect_error(guess.transform.format(unknown_file), "format")
  expect_error(guess.transform.format(file.path(tempdir(), "does_not_exist_at_all.xfm")))
  unlink(c(mat_file, no_extension, itk_file, itk_mat, unknown_file))
})


test_that("An FSL matrix can be written, and only if it can be represented.", {
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(lta_file)
  mat_file <- tempfile(fileext = ".mat")

  write.fs.transform(tf, mat_file) # the format is derived from the file extension
  expect_true(file.exists(mat_file))
  matrix_lines <- readLines(mat_file)
  expect_equal(length(matrix_lines), 4L)
  expect_true(all(sapply(matrix_lines, text.line.is.numeric, num = 4L)))
  expect_equal(read.fs.transform(mat_file)$matrix, tf$matrix) # lossless round trip

  # A transformation in world coordinates cannot be written as an FSL matrix: the format does not store the
  # volumes, so the meaning of the matrix would change silently.
  world_tf <- transform.to.world(tf)
  expect_error(write.fs.transform(world_tf, mat_file, format = "fslmat"), "transform.to.voxel")
  expect_error(write.fs.transform.fslmat(world_tf, mat_file), "transform.to.voxel")

  # Same for a transformation that uses one-based voxel indices.
  one_based <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 1L)
  expect_error(write.fs.transform.fslmat(one_based, mat_file), "zero-based")

  # Unsupported and undetectable formats.
  expect_error(write.fs.transform(tf, mat_file, format = "nonsense"), "not supported")
  expect_error(write.fs.transform(tf, tempfile(fileext = ".tfm")))
  expect_error(write.fs.transform("not a transform", mat_file))
  unlink(mat_file)
})


test_that("The FSL world space matrix follows the convention of FSL, MRtrix3 and FreeSurfer.", {
  # A volume whose header already uses the FSL (radiological) convention is used as it is, with the voxel
  # sizes divided out.
  negative_det <- list("vox2ras" = diag(c(-2, 2, 2, 1)), "dim" = c(100L, 100L, 100L), "voxelsize" = c(2, 2, 2))
  expect_equal(fsl.scaled.voxel.matrix(negative_det), diag(c(-1, 1, 1, 1)))

  # A volume with a positive determinant gets its first axis flipped, which moves the origin to the other end
  # of the volume.
  positive_det <- list("vox2ras" = diag(4), "dim" = c(100L, 100L, 100L), "voxelsize" = c(1, 1, 1))
  flipped <- fsl.scaled.voxel.matrix(positive_det)
  expect_equal(flipped[1L, 1L], -1.0)
  expect_equal(flipped[1L, 4L], 99.0)
  expect_equal(flipped[2L, 2L], 1.0)
  expect_equal(flipped[3L, 3L], 1.0)

  # In both cases the result has unit axes and a negative determinant.
  for (geometry in list(negative_det, positive_det)) {
    scaled <- fsl.scaled.voxel.matrix(geometry)
    expect_equal(sqrt(colSums(scaled[1:3, 1:3]^2)), c(1, 1, 1))
    expect_true(det(scaled[1:3, 1:3]) < 0.0)
  }
})


test_that("The geometry of a volume can be determined from various inputs.", {
  brain_image <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  volume <- read.fs.volume(brain_image, with_header = TRUE)

  geometry <- volume.geometry(volume)
  expect_equal(geometry$dim, dim(volume$data)[1:3])
  expect_equal(geometry$vox2ras, mghheader.vox2ras(volume$header))
  expect_equal(geometry$voxelsize, sqrt(colSums(geometry$vox2ras[1:3, 1:3]^2)))

  # The header of a volume can be passed instead of the volume itself.
  expect_equal(volume.geometry(volume$header)$vox2ras, geometry$vox2ras)

  expect_error(volume.geometry("not a volume"))
  expect_error(volume.geometry(list("foo" = 1)))
  expect_error(volume.geometry(list()))

  skip_if_not_installed("oro.nifti")
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  nii <- oro.nifti::readNIfTI(nii_file, reorient = FALSE)
  if (nii@sform_code > 0L) {
    # The 'nifti' instance must give the same geometry as the package reader, which uses the same 'sform'.
    fs_volume <- read.fs.volume.nii(nii_file, with_header = TRUE)
    geometry_nii <- volume.geometry(nii)
    expect_equal(geometry_nii$vox2ras, fs_volume$header$vox2ras_matrix)
    expect_equal(geometry_nii$dim, dim(fs_volume$data)[1:3])
  } else {
    expect_error(volume.geometry(nii)) # a NIfTI without an sform cannot be used
  }
})


test_that("A transformation can be converted between voxel and world coordinates.", {
  # An LTA records the geometry of the volumes it relates, so no volumes have to be passed.
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(lta_file)
  world_tf <- transform.to.world(tf)

  expect_true(is.fs.transform(world_tf))
  expect_equal(world_tf$format, "lta") # provenance is kept
  expect_equal(world_tf$space_in, "ras")
  expect_equal(world_tf$space_out, "ras")
  expect_true(is.na(world_tf$voxel_base))
  expect_equal(world_tf$src$frame, "scanner")
  expect_equal(world_tf$dst$frame, "scanner")
  expect_equal(world_tf$matrix, tf$dst$vox2ras %*% tf$matrix %*% solve(tf$src$vox2ras))

  back_to_voxel <- transform.to.voxel(world_tf)
  expect_equal(back_to_voxel$matrix, tf$matrix)
  expect_equal(back_to_voxel$space_in, "voxel")
  expect_equal(back_to_voxel$voxel_base, 0L)

  # A transformation that already has the requested spaces is returned unchanged.
  expect_equal(transform.to.world(world_tf), world_tf)
  expect_equal(transform.to.voxel(tf), tf)

  # An FSL matrix does not record the volumes, so they are required.
  mat_file <- tempfile(fileext = ".mat")
  write.fs.transform(tf, mat_file, format = "fslmat")
  fsl_tf <- read.fs.transform(mat_file)
  expect_error(transform.to.world(fsl_tf), "both volumes")
  expect_error(transform.to.world(fsl_tf, src = read.fs.volume(system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)), "both volumes")

  src_volume <- read.fs.volume(system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  dst_volume <- read.fs.volume(system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  fsl_world <- transform.to.world(fsl_tf, src = src_volume, dst = dst_volume)

  expect_equal(fsl_world$space_in, "ras")
  expect_equal(fsl_world$src$frame, "fsl") # the FSL world space, not the header RAS space
  expect_equal(fsl_world$dst$frame, "fsl")
  expect_equal(
    fsl_world$matrix,
    fsl.scaled.voxel.matrix(volume.geometry(dst_volume)) %*% fsl_tf$matrix %*% solve(fsl.scaled.voxel.matrix(volume.geometry(src_volume)))
  )

  # Converting back gives the original matrix, and the volumes are not needed a second time because the
  # resulting transformation records the geometry it uses.
  fsl_back <- transform.to.voxel(fsl_world)
  expect_equal(fsl_back$matrix, fsl_tf$matrix)
  expect_equal(fsl_back$voxel_base, 0L)
  expect_equal(transform.to.voxel(fsl_world)$matrix, fsl_tf$matrix)

  expect_error(transform.to.world("not a transform"))
  expect_error(transform.to.voxel("not a transform"))
  unlink(mat_file)
})
