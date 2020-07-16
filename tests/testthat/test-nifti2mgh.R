

test_that("We can load a NIFTI volume file as an MGH instance", {

  nii_file = system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.volume.nii(nii_file, with_header = TRUE);
  mgh2 = read.fs.volume(nii_file, with_header = TRUE);

  expect_true(is.fs.volume(mgh));
  expect_true(is.fs.volume(mgh2));
  expect_equal(dim(mgh$data), c(3, 3, 3, 1));
  expect_equal(dim(mgh2$data), c(3, 3, 3, 1));

  expect_error(read.fs.volume(nii_file, format = 'no such format')); # invalid format
})


