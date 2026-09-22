# Tests for the fs.transform object, the common representation of transformation matrices.

test_that("An fs.transform instance can be created by the constructor.", {
  tf <- fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras")
  expect_true(is.fs.transform(tf))
  expect_equal(tf$matrix, diag(4))
  expect_equal(tf$space_in, "ras")
  expect_equal(tf$space_out, "ras")
  expect_true(is.na(tf$voxel_base))
  expect_true(is.null(tf$src))
  expect_true(is.null(tf$dst))
  expect_true(is.na(tf$format))
  expect_true(is.null(tf$source))
  expect_true(is.null(tf$type))
})


test_that("The fs.transform validator rejects invalid instances.", {
  expect_error(validate.fs.transform("not a transform"))
  expect_error(fs.transform(matrix = diag(3L), space_in = "ras", space_out = "ras")) # not 4x4
  expect_error(fs.transform(matrix = "no matrix", space_in = "ras", space_out = "ras"))
  expect_error(fs.transform(matrix = matrix(NA_real_, nrow = 4L, ncol = 4L), space_in = "ras", space_out = "ras"))
  expect_error(fs.transform(matrix = diag(4), space_in = "world", space_out = "ras")) # unknown space
  expect_error(fs.transform(matrix = diag(4), space_in = c("ras", "ras"), space_out = "ras"))
  expect_error(fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel")) # voxel_base missing
  expect_error(fs.transform(matrix = diag(4), space_in = "voxel", space_out = "ras", voxel_base = 2L))
  expect_error(fs.transform(matrix = diag(4), space_in = "ras", space_out = "lps", voxel_base = 0L)) # meaningless
  expect_error(fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", src = "not a list"))
  expect_error(fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", dst = list(frame = "unknown")))
  expect_error(fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", dst = list(dim = 128L)))
  expect_error(fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", dst = list(vox2ras = diag(3L))))

  # These are valid.
  expect_true(validate.fs.transform(fs.transform(matrix = diag(4), space_in = "voxel", space_out = "ras", voxel_base = 0L)))
  expect_true(validate.fs.transform(fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", voxel_base = NA_integer_, format = "xfm")))
})


test_that("is.fs.transform only accepts fs.transform instances.", {
  tf <- read.fs.transform(system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE))
  expect_true(is.fs.transform(tf))
  expect_false(is.fs.transform(list(matrix = diag(4))))
  expect_false(is.fs.transform("no transform"))
  expect_false(is.fs.transform(NULL))
})


test_that("An xfm file is read as a RAS to RAS transformation.", {
  tf <- read.fs.transform(system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE))
  expect_true(is.fs.transform(tf))
  expect_equal(tf$format, "xfm")
  expect_equal(tf$space_in, "ras")
  expect_equal(tf$space_out, "ras")
  expect_true(is.na(tf$voxel_base))
  expect_true(is.null(tf$src))
  expect_true(is.null(tf$dst))
  expect_false(is.null(tf$source))
  expect_equal(tf$type, "Linear")
})


test_that("A register.dat file is read as a voxel to tkregister RAS transformation.", {
  tf <- read.fs.transform(system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE))
  expect_true(is.fs.transform(tf))
  expect_equal(tf$format, "dat")
  expect_equal(tf$space_in, "voxel")
  expect_equal(tf$space_out, "ras")
  expect_equal(tf$voxel_base, 0L) # FreeSurfer voxel indices are zero-based
  expect_true(is.null(tf$src))
  expect_equal(tf$dst$frame, "tkreg")
  expect_equal(tf$intensity, 0.15) # used to be read as an integer, i.e., 0
})


test_that("An LTA file is read with the spaces stated by its type, along with both volumes.", {
  tf <- read.fs.transform(system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE))
  expect_true(is.fs.transform(tf))
  expect_equal(tf$format, "lta")
  expect_equal(tf$type, "0") # LINEAR_VOX_TO_VOX
  expect_equal(tf$space_in, "voxel")
  expect_equal(tf$space_out, "voxel")
  expect_equal(tf$voxel_base, 0L)

  for (volume_info in list(tf$src, tf$dst)) {
    expect_false(is.null(volume_info))
    expect_true(is.null(volume_info$path) || is.character(volume_info$path))
    expect_equal(length(volume_info$dim), 3L)
    expect_equal(length(volume_info$voxelsize), 3L)
    expect_equal(dim(volume_info$vox2ras), c(4L, 4L))
    expect_equal(volume_info$frame, "scanner")
  }
  expect_equal(tf$src$path, "nu.mgz")
  expect_equal(tf$src$dim, c(256L, 256L, 256L))

  # The parsed file content is preserved.
  expect_equal(tf$header$nxforms, "1")
  expect_equal(tf$volumes$dst$filename, "/Applications/freesurfer/average/RB_all_2016-05-10.vc700.gca")
})


test_that("A volume descriptor computes vox2ras in the convention of mghheader.vox2ras.", {
  mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  volume <- read.fs.mgh(mgz_file, with_header = TRUE)
  header <- volume$header

  voxelsize <- c(header$internal$xsize, header$internal$ysize, header$internal$zsize)
  dims <- c(header$internal$width, header$internal$height, header$internal$depth)
  cras <- c(header$internal$c_r, header$internal$c_a, header$internal$c_s)
  mdc_scaled <- matrix(c(
    header$internal$x_r, header$internal$x_a, header$internal$x_s,
    header$internal$y_r, header$internal$y_a, header$internal$y_s,
    header$internal$z_r, header$internal$z_a, header$internal$z_s
  ), nrow = 3L, byrow = FALSE) %*% diag(voxelsize)

  desc <- volume.descriptor(
    path = "brain.mgz", dim = dims, voxelsize = voxelsize,
    xras = mdc_scaled[, 1L], yras = mdc_scaled[, 2L], zras = mdc_scaled[, 3L], cras = cras
  )
  expect_equal(desc$vox2ras, mghheader.vox2ras(header))

  # 'cras' is the RAS coordinate of the center of the volume, i.e., of voxel index dim/2, and not the
  # translation column of the voxel-to-RAS matrix.
  center_ras <- as.numeric((desc$vox2ras %*% c(dims / 2.0, 1.0))[1:3])
  expect_equal(center_ras, cras)

  # A descriptor without any information is NULL, and the voxel-to-RAS matrix cannot be derived from the
  # direction vectors alone.
  expect_true(is.null(volume.descriptor()))
  expect_warning(volume.descriptor(xras = c(1, 0, 0), yras = c(0, 1, 0), zras = c(0, 0, 1), cras = c(0, 0, 0)))
  expect_error(volume.descriptor(vox2ras = diag(3L)))
  expect_error(volume.descriptor(frame = "nonsense"))
})


test_that("A transformation can be inverted.", {
  tf <- read.fs.transform(system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE))
  tf_back <- invert.fs.transform(tf)

  expect_true(is.fs.transform(tf_back))
  expect_equal(tf_back$matrix, solve(tf$matrix))
  expect_true(max(abs(tf_back$matrix %*% tf$matrix - diag(4))) < 1e-10)
  expect_equal(tf_back$space_in, tf$space_out)
  expect_equal(tf_back$space_out, tf$space_in)
  expect_equal(tf_back$src, tf$dst)
  expect_equal(tf_back$dst, tf$src)
  expect_equal(tf_back$format, tf$format) # provenance is kept

  # Swapping the spaces does not change the voxel base, since both sides of an fs.transform use the same one.
  voxel_tf <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "ras", voxel_base = 0L)
  voxel_tf_back <- invert.fs.transform(voxel_tf)
  expect_equal(voxel_tf_back$space_in, "ras")
  expect_equal(voxel_tf_back$space_out, "voxel")
  expect_equal(voxel_tf_back$voxel_base, 0L)

  expect_error(invert.fs.transform("not a transform"))
  expect_error(invert.fs.transform(fs.transform(matrix = diag(c(0, 1, 1, 1)), space_in = "ras", space_out = "ras")))
})


test_that("Printing and summarizing a transform reports its spaces.", {
  tf <- read.fs.transform(system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE))

  output <- capture.output(printed <- print(tf))
  expect_true(is.fs.transform(printed))
  expect_true(any(grepl("format 'lta'", output)))
  expect_true(any(grepl("voxel coordinates (0-based)", output, fixed = TRUE)))
  expect_true(any(grepl("nu.mgz", output)))
  expect_true(any(grepl("Determinant", output)))

  s <- summary(tf)
  expect_equal(s$format, "lta")
  expect_equal(s$space_in, "voxel")
  expect_equal(s$space_out, "voxel")
  expect_equal(s$voxel_base, 0L)
  expect_equal(s$determinant, det(tf$matrix))
  expect_equal(unname(s$translation), unname(tf$matrix[1:3, 4L]))
  expect_true(s$is_affine)
  expect_true(s$is_invertible)
  expect_equal(s$src, tf$src)

  # The spaces of a transform of unknown space are reported as unknown.
  unknown_tf <- fs.transform(matrix = diag(4), space_in = NA_character_, space_out = NA_character_)
  expect_true(any(grepl("unknown coordinates", capture.output(print(unknown_tf)))))
})


test_that("An LTA file without a transformation type is read with unknown spaces.", {
  lta_file <- tempfile(fileext = ".lta")
  matrix_lines <- c("1 0 0 0", "0 1 0 0", "0 0 1 0", "0 0 0 1")

  writeLines(c("nxforms   = 1", "1 4 4", matrix_lines), lta_file)
  expect_warning(tf <- read.fs.transform(lta_file), "does not state a transformation type")
  expect_true(is.fs.transform(tf))
  expect_equal(tf$matrix, diag(4))
  expect_true(is.na(tf$space_in))
  expect_true(is.na(tf$space_out))
  expect_true(is.na(tf$voxel_base))
  expect_true(is.null(tf$src))
  expect_true(is.null(tf$dst))

  # Type 1 is LINEAR_RAS_TO_RAS: RAS world coordinates on both sides, so there is no voxel base.
  writeLines(c("type      = 1", "nxforms   = 1", "1 4 4", matrix_lines), lta_file)
  tf_ras <- read.fs.transform(lta_file)
  expect_equal(tf_ras$type, "1")
  expect_equal(tf_ras$space_in, "ras")
  expect_equal(tf_ras$space_out, "ras")
  expect_true(is.na(tf_ras$voxel_base))

  # An unknown type is reported instead of guessed.
  writeLines(c("type      = 7", "nxforms   = 1", "1 4 4", matrix_lines), lta_file)
  expect_warning(tf_unknown <- read.fs.transform(lta_file), "unsupported transformation type 7")
  expect_true(is.na(tf_unknown$space_in))
  expect_true(is.na(tf_unknown$voxel_base))

  # An LTA file without a matrix is reported as such.
  writeLines(c("type      = 0", "nxforms   = 1"), lta_file)
  expect_error(read.fs.transform(lta_file))
  unlink(lta_file)
})
