test_that("We can load a NIFTI volume file as an MGH instance", {
  nii_file <- system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE)
  mgh <- read.fs.volume.nii(nii_file, with_header = TRUE)
  mgh2 <- read.fs.volume(nii_file, with_header = TRUE)

  expect_true(is.fs.volume(mgh))
  expect_true(is.fs.volume(mgh2))
  expect_equal(dim(mgh$data), c(3, 3, 3, 1))
  expect_equal(dim(mgh2$data), c(3, 3, 3, 1))

  mgh_flat <- read.fs.volume(nii_file, with_header = TRUE, flatten = TRUE)
  expect_equal(length(mgh_flat$data), 27)
  expect_true(is.null(dim(mgh_flat$data)))

  mgh_dropped <- read.fs.volume(nii_file, with_header = TRUE, drop_empty_dims = TRUE)
  expect_equal(dim(mgh_dropped$data), c(3, 3, 3))
  expect_true(is.array(read.fs.volume(nii_file, with_header = FALSE)))

  expect_error(read.fs.volume.nii(nii_file, format = "no such format")) # invalid format
  expect_error(read.fs.volume.nii(124)) # invalid filepath: neither strings nor nifti instance
})


test_that("We can compute mghheader data from the q-form in a NIFTI volume file", {
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  nifti_img <- oro.nifti::readNIfTI(nii_file, reorient = FALSE)
  nifti_img@sform_code <- 0L # set sform to zero to force reading the qform
  mgh <- read.fs.volume.nii(nifti_img, with_header = TRUE)
  expect_true(is.fs.volume(mgh))
})


test_that("Warnings are show if neither s-form nor  q-form are available in a NIFTI volume file", {
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  nifti_img <- oro.nifti::readNIfTI(nii_file, reorient = FALSE)
  nifti_img@sform_code <- 0L # set sform to zero
  nifti_img@qform_code <- 0L # set qform to zero
  expect_warning(read.fs.volume.nii(nifti_img, with_header = TRUE))
})


test_that("A NIFTI file is read with the NIFTI reader of this package", {
  # The file is read by the readers of this package, which means that the returned data must match the raw data
  # that the low-level reader returns, except for the data scaling fields, which are applied while reading.
  reference_file <- system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE)
  raw_data <- read.nifti1.data(reference_file)

  nii_file <- tempfile(fileext = ".nii")
  write.nifti1(nii_file, raw_data, read.nifti1.header(reference_file))
  expect_equal(as.vector(read.fs.volume.nii(nii_file)), as.vector(raw_data))
  expect_equal(dim(read.fs.volume.nii(nii_file)), c(3L, 3L, 3L, 1L))

  # The 'scl_slope'/'scl_inter' header fields are applied to the values while reading a file (this is what
  # oro.nifti did, and what the 'rescale_data' parameter of oro.nifti::readNIfTI does by default).
  header <- read.nifti1.header(reference_file)
  header$scl_slope <- 2.0
  header$scl_inter <- 10.0
  nii_file <- tempfile(fileext = ".nii")
  write.nifti1(nii_file, raw_data, header)
  expect_equal(as.vector(read.fs.volume.nii(nii_file)), as.vector(raw_data) * 2.0 + 10.0)
  # The low-level reader returns the values as they are stored in the file, without the scaling.
  expect_equal(as.vector(read.nifti1.data(nii_file)), as.vector(raw_data))
})


test_that("A NIFTI file without an extension can be read", {
  reference_file <- system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE)
  base_name <- tempfile()
  file.copy(reference_file, paste0(base_name, ".nii"))
  expect_equal(dim(read.fs.volume.nii(base_name)), c(3L, 3L, 3L, 1L))
})


test_that("A NIFTI v2 file is read", {
  data <- array(1:12, dim = c(2L, 3L, 2L))
  nii_file <- tempfile(fileext = ".nii")
  write.nifti2(nii_file, data, ni2header.for.data(data))
  expect_warning(volume <- read.fs.volume.nii(nii_file), "does not contain valid sform")
  expect_equal(dim(volume), c(2L, 3L, 2L, 1L))
  expect_equal(drop(volume), data)
})


test_that("Voxel sizes of 0 are reported as 1, like oro.nifti did", {
  # Such files are not valid, but they are written by this package itself: the header template returned by
  # ni1header.for.data() stores a pixdim of 0. The entries are replaced for the used data dimensions, which is
  # what oro.nifti did. See the note in the docs of read.fs.volume.nii.
  data <- array(1:24, dim = c(2L, 3L, 2L, 2L))
  nii_file <- tempfile(fileext = ".nii")
  write.nifti1(nii_file, data, ni1header.for.data(data))
  expect_equal(nifti.info.from.file(nii_file)$pixdim[2:5], c(1., 1., 1., 1.))
})
