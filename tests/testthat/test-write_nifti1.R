
test_that("NIFTI v1 files can be written based on integer data using write.nifti1", {

  int_data = array(data = seq.int(100L), dim = c(50, 2));

  int_nifti_file = tempfile(fileext = ".nii");
  nifti_written = write.nifti1(int_nifti_file, int_data);

  testthat::expect_equal(nifti_written$header$bitpix, 32L);
  testthat::expect_equal(nifti_written$header$datatype, 8L);

  header_reread = read.nifti1.header(int_nifti_file);
  data_reread = read.nifti1.data(int_nifti_file);
  testthat::expect_equal(dim(data_reread), c(50, 2));
  testthat::expect_equal(int_data, data_reread);
})


test_that("NIFTI v1 files can be written based on double data using write.nifti1", {

  double_data = array(data=rnorm(100L, 50.0, 4.0), dim = c(50, 2));
  double_nifti_file = tempfile(fileext = ".nii");

  nifti_written = write.nifti1(double_nifti_file, double_data);
  testthat::expect_equal(nifti_written$header$bitpix, 32L);
  testthat::expect_equal(nifti_written$header$datatype, 16L);

  header_reread = read.nifti1.header(double_nifti_file);
  data_reread = read.nifti1.data(double_nifti_file);
  testthat::expect_equal(dim(data_reread), c(50, 2));
  testthat::expect_equal(double_data, data_reread);
})

