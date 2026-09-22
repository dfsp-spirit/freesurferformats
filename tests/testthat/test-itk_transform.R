# Tests for reading and writing ITK text transforms, and for the LPS/RAS conversion.
#
# The real files used by some tests are in the 'extra_test_data/transforms' directory of the repository, which is
# excluded from the built package (see '.Rbuildignore'). They are derived from the CC0-licensed dataset that is
# also the source of the other files in 'extra_test_data':
#   - 'ants_affine_double_nonzero_centre.txt' is written by the ANTs tool 'ConvertTransformFile' (via
#     QSIRECON/nipype), and has a non-zero center of rotation.
#   - 'fmriprep_fsnative_to_T1w_float.txt' and 'fmriprep_T1w_to_fsnative_float.txt' are a forward and inverse
#     pair written by fMRIPrep (via nipype's 'lta2itk'), with a zero center of rotation.

itk_test_file <- function(lines, extension = ".tfm") {
  filepath <- tempfile(fileext = extension)
  writeLines(lines, filepath)
  return(filepath)
}

# An affine with a non-zero center of rotation, to check that the center is folded into the translation. The
# linear part is not symmetric, which also distinguishes row-major from column-major parameter order.
itk_affine_lines <- function(params = NULL, fixed = "10 20 30", transform_class = "AffineTransform_float_3_3") {
  if (is.null(params)) {
    params <- c(1, 2, 0, 0, 1, 0, 0, 0, 1, 1, 2, 3)
  }
  return(c(
    "#Insight Transform File V1.0",
    "#Transform 0",
    sprintf("Transform: %s", transform_class),
    sprintf("Parameters: %s", paste(format(params, digits = 12, trim = TRUE), collapse = " ")),
    sprintf("FixedParameters: %s", fixed)
  ))
}

test_that("An ITK text transform can be read.", {
  filepath <- itk_test_file(itk_affine_lines())

  tf <- read.fs.transform(filepath)
  expect_true(is.fs.transform(tf))
  expect_equal(tf$format, "itk")
  expect_equal(tf$space_in, "lps") # ITK world coordinates are left-posterior-superior
  expect_equal(tf$space_out, "lps")
  expect_true(is.na(tf$voxel_base))
  expect_true(is.null(tf$src))
  expect_true(is.null(tf$dst))
  expect_equal(tf$type, "AffineTransform_float_3_3")
  expect_equal(tf$fixed_parameters, c(10, 20, 30))
  expect_equal(tf$source, filepath)

  # The linear part is row-major, and the center of rotation c is folded into the translation:
  # y = A(x - c) + t + c.
  linear <- matrix(c(1, 2, 0, 0, 1, 0, 0, 0, 1), ncol = 3L, byrow = TRUE)
  translation <- c(1, 2, 3)
  center <- c(10, 20, 30)
  expected <- rbind(cbind(linear, translation + center - (linear %*% center)), c(0, 0, 0, 1))
  expect_equal(tf$matrix, expected)
  expect_false(isTRUE(all.equal(tf$matrix[1:3, 4L], translation))) # the center really does change it

  # The same with double precision and with the other supported class name.
  for (transform_class in c("AffineTransform_double_3_3", "MatrixOffsetTransformBase_double_3_3", "MatrixOffsetTransformBase_float_3_3")) {
    variant <- read.fs.transform(itk_test_file(itk_affine_lines(transform_class = transform_class)))
    expect_equal(variant$matrix, expected)
    expect_equal(variant$type, transform_class)
  }

  # A zero center leaves the translations as they are, as in the files that fMRIPrep writes.
  zero_center <- read.fs.transform(itk_test_file(itk_affine_lines(fixed = "0 0 0")))
  expect_equal(zero_center$matrix[1:3, 4L], c(1, 2, 3))

  expect_equal(read.fs.transform(filepath, format = "itk")$matrix, expected)
  unlink(filepath)
})


test_that("Unsupported ITK transforms are reported instead of being misread.", {
  # A class whose parameters are not an affine matrix: reading its 12 values as one would be a silent error.
  for (unsupported in c("Euler3DTransform_float_3_3", "TranslationTransform_double_3_3", "BSplineTransform_double_3_3", "VersorRigid3DTransform_double_3_3")) {
    expect_error(
      read.fs.transform(itk_test_file(itk_affine_lines(transform_class = unsupported))),
      "Unsupported ITK transform class"
    )
  }

  # Several transformations in one file (an ITK composite transform), as fMRIPrep writes them for motion
  # correction. Composing them would need ITK's ordering rules, so this is refused rather than guessed.
  multi_block <- c(
    "#Insight Transform File V1.0",
    "#Transform 0", "Transform: AffineTransform_float_3_3", "Parameters: 1 0 0 0 1 0 0 0 1 0 0 0", "FixedParameters: 0 0 0",
    "#Transform 1", "Transform: AffineTransform_float_3_3", "Parameters: 1 0 0 0 1 0 0 0 1 1 1 1", "FixedParameters: 0 0 0"
  )
  expect_error(read.fs.transform(itk_test_file(multi_block)), "2 transformations")

  # Missing or malformed entries.
  expect_error(read.fs.transform(itk_test_file(itk_affine_lines(fixed = NULL))), "FixedParameters")
  expect_error(read.fs.transform(itk_test_file(c(
    "#Insight Transform File V1.0", "#Transform 0", "Transform: AffineTransform_float_3_3",
    "Parameters: 1 2 3", "FixedParameters: 0 0 0"
  )), "12 parameters"))
  expect_error(read.fs.transform(itk_test_file(c(
    "#Insight Transform File V1.0", "#Transform 0", "Parameters: 1 2 3", "FixedParameters: 0 0 0"
  )), "Transform"))
  expect_error(read.fs.transform(itk_test_file(c("#Insight Transform File V1.0", "no entries here"))), "no '#Transform' block")
  expect_error(read.fs.transform(itk_test_file(c("not an ITK file", "at all"))), "not an ITK text transform")

  # A binary ITK transform, e.g. the MATLAB '.mat' file that ANTs writes by default. These share the extension
  # with FSL matrices and are detected from their content.
  binary_file <- tempfile(fileext = ".mat")
  writeBin(c(as.raw(0L), charToRaw("AffineTransform_double_3_3"), as.raw(0L), charToRaw("fixed")), binary_file)
  expect_error(read.fs.transform(binary_file), "binary ITK or ANTs transform")
  unlink(binary_file)
})


test_that("Real ITK transforms written by other tools can be read.", {
  ants_file <- find_extra_test_data_file("transforms/ants_affine_double_nonzero_centre.txt")
  forward_file <- find_extra_test_data_file("transforms/fmriprep_fsnative_to_T1w_float.txt")
  inverse_file <- find_extra_test_data_file("transforms/fmriprep_T1w_to_fsnative_float.txt")
  skip_if(is.null(ants_file) || is.null(forward_file) || is.null(inverse_file), "Test data missing.")

  ants_tf <- read.fs.transform(ants_file)
  expect_equal(ants_tf$type, "AffineTransform_double_3_3")
  expect_equal(ants_tf$space_in, "lps")
  # The ANTs file has a non-zero center of rotation, which must have been folded into the translation.
  expect_false(all(ants_tf$fixed_parameters[1:3] == 0))

  forward_tf <- read.fs.transform(forward_file)
  inverse_tf <- read.fs.transform(inverse_file)
  expect_equal(forward_tf$type, "AffineTransform_float_3_3")
  expect_equal(forward_tf$fixed_parameters[1:3], c(0, 0, 0))

  # The two fMRIPrep files are a forward and an inverse transformation, so both must invert each other. This
  # checks the whole chain from the file to the matrix, including the handedness and the parameter order.
  expect_true(max(abs(forward_tf$matrix %*% inverse_tf$matrix - diag(4))) < 1e-5)

  # The matrices are in LPS coordinates, and converting them to RAS must not change anything else.
  for (tf in list(ants_tf, forward_tf, inverse_tf)) {
    ras_tf <- transform2ras(tf)
    expect_equal(ras_tf$space_in, "ras")
    expect_equal(ras_tf$matrix, diag(c(-1, -1, 1, 1)) %*% tf$matrix %*% diag(c(-1, -1, 1, 1)))
    expect_equal(transform2lps(ras_tf)$matrix, tf$matrix)
  }
})


test_that("An ITK text transform can be written and read back.", {
  xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
  tf <- transform2lps(read.fs.transform(xfm_file))
  out_file <- tempfile(fileext = ".tfm")

  write.fs.transform.itk(tf, out_file)
  back <- read.fs.transform(out_file)
  expect_identical(back$matrix, tf$matrix) # lossless, the values are written with 17 digits
  expect_equal(back$space_in, "lps")
  expect_equal(back$type, "AffineTransform_double_3_3")

  file_lines <- readLines(out_file)
  expect_equal(file_lines[1L], "#Insight Transform File V1.0")
  expect_true(any(grepl("^Transform: AffineTransform_double_3_3$", file_lines)))
  expect_true(any(grepl("^Parameters: ", file_lines)))
  expect_true(any(grepl("^FixedParameters: 0 0 0$", file_lines)))

  # The parameters are the row-major linear part followed by the translation.
  parameters <- as.numeric(strsplit(sub("^Parameters: ", "", file_lines[startsWith(file_lines, "Parameters: ")][1L]), "[[:space:]]+")[[1L]])
  expect_equal(matrix(parameters[1:9], ncol = 3L, byrow = TRUE), tf$matrix[1:3, 1:3])
  expect_equal(parameters[10:12], tf$matrix[1:3, 4L])

  # ITK transforms operate on LPS coordinates, so a transformation in RAS coordinates has to be converted, and a
  # non-affine matrix cannot be stored in this format.
  expect_error(write.fs.transform.itk(read.fs.transform(xfm_file), out_file), "transform2lps")
  non_affine <- diag(4L)
  non_affine[4L, ] <- c(0, 0, 1, 5)
  expect_error(write.fs.transform.itk(fs.transform(matrix = non_affine, space_in = "lps", space_out = "lps"), out_file), "affine")

  # The writer is reachable through the generic function, from the extension.
  write.fs.transform(tf, out_file)
  expect_equal(read.fs.transform(out_file)$matrix, tf$matrix)

  expect_error(write.fs.transform.itk("not a transform", out_file))
  unlink(out_file)
})


test_that("The ITK format is detected from the content and the extension.", {
  tf_file <- itk_test_file(itk_affine_lines())
  expect_equal(guess.transform.format(tf_file), "itk")

  # ITK transforms are usually written as '.tfm' or '.txt', and the content decides for '.txt'.
  tfm_file <- itk_test_file(itk_affine_lines(), extension = ".tfm")
  txt_file <- itk_test_file(itk_affine_lines(), extension = ".txt")
  expect_equal(guess.transform.format(tfm_file), "itk")
  expect_equal(guess.transform.format(txt_file), "itk")
  expect_equal(read.fs.transform(txt_file)$space_in, "lps")

  # An FSL matrix is still detected as such, even though '.mat' is also used for binary ITK transforms.
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  mat_file <- tempfile(fileext = ".mat")
  write.fs.transform(read.fs.transform(lta_file), mat_file, format = "fslmat")
  expect_equal(guess.transform.format(mat_file), "fslmat")
  unlink(c(tf_file, tfm_file, txt_file, mat_file))
})


test_that("A transformation can be converted between the RAS and the LPS convention.", {
  xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(xfm_file) # RAS

  lps_tf <- transform2lps(tf)
  expect_equal(lps_tf$space_in, "lps")
  expect_equal(lps_tf$space_out, "lps")
  expect_equal(lps_tf$matrix, diag(c(-1, -1, 1, 1)) %*% tf$matrix %*% diag(c(-1, -1, 1, 1)))
  # The conversion is its own inverse, and a transformation that is already in the requested convention is
  # returned unchanged.
  expect_equal(transform2ras(lps_tf)$matrix, tf$matrix)
  expect_equal(transform2ras(tf), tf)
  expect_equal(transform2lps(lps_tf), lps_tf)
  expect_equal(lps_tf$format, tf$format) # provenance is kept

  # The conversion needs world coordinates, since the convention is a property of them.
  voxel_tf <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 0L)
  expect_error(transform2ras(voxel_tf), "transform2world")

  # And the voxel conversion refuses an LPS transformation, since the volume geometry of this package describes
  # RAS coordinates: silently mixing the two would produce wrong coordinates.
  src_volume <- read.fs.volume(system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  dst_volume <- read.fs.volume(system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  expect_error(transform2voxel(lps_tf, src = src_volume, dst = dst_volume), "transform2ras")

  expect_error(transform2ras("not a transform"))
  expect_error(transform2lps("not a transform"))
})
