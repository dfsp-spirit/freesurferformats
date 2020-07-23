

test_that("We can load a NIFTI volume file as an MGH instance", {

  nii_file = system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.volume.nii(nii_file, with_header = TRUE);
  mgh2 = read.fs.volume(nii_file, with_header = TRUE);

  expect_true(is.fs.volume(mgh));
  expect_true(is.fs.volume(mgh2));
  expect_equal(dim(mgh$data), c(3, 3, 3, 1));
  expect_equal(dim(mgh2$data), c(3, 3, 3, 1));

  mgh_flat = read.fs.volume(nii_file, with_header = TRUE, flatten = TRUE);
  expect_equal(length(mgh_flat$data), 27);
  expect_true(is.null(dim(mgh_flat$data)));

  mgh_dropped = read.fs.volume(nii_file, with_header = TRUE, drop_empty_dims = TRUE);
  expect_equal(dim(mgh_dropped$data), c(3, 3, 3));
  expect_true(is.array(read.fs.volume(nii_file, with_header = FALSE)));

  expect_error(read.fs.volume.nii(nii_file, format = 'no such format')); # invalid format
  expect_error(read.fs.volume.nii(124)); # invalid filepath: neither strings nor nifti instance
})


test_that("We can compute mghheader data from the q-form in a NIFTI volume file", {
  nii_file = system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE);
  nifti_img = oro.nifti::readNIfTI(nii_file);
  nifti_img@sform_code = 0L; # set sform to zero to force reading the qform
  mgh = read.fs.volume.nii(nifti_img, with_header = TRUE);
  expect_true(is.fs.volume(mgh));
})


test_that("Warnings are show if neither s-form nor  q-form are available in a NIFTI volume file", {
  nii_file = system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE);
  nifti_img = oro.nifti::readNIfTI(nii_file);
  nifti_img@sform_code = 0L; # set sform to zero
  nifti_img@qform_code = 0L; # set qform to zero
  expect_warning(read.fs.volume.nii(nifti_img, with_header = TRUE));
})



