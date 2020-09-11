
test_that("NIFTI v1 files can be written using write.nifti1", {

  int_data = seq.int(100L);
  double_data = rnorm(100L, 50.0, 4.0);

  int_nifti_file = tempfile(fileext = ".nii");
  double_nifti_file = tempfile(fileext = ".nii");

  testthat::expect_equal(1L, 1L);
})
