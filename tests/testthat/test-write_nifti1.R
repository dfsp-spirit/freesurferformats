
test_that("NIFTI v1 files can be written based on integer data using write.nifti1", {

  int_data = array(data = seq.int(100L), dim = c(50, 2));

  int_nifti_file = tempfile(fileext = ".nii");
  nifti_written = write.nifti1(int_nifti_file, int_data);
  testthat::expect_equal(nifti_written$header$cal_max, 100);
  testthat::expect_equal(nifti_written$header$cal_min, 1);
  testthat::expect_equal(nifti.datadim.from.dimfield(nifti_written$header$dim), c(50, 2));
  testthat::expect_equal(nifti_written$header$dim, c(2, 50, 2, 1, 1, 1, 1, 1));
  testthat::expect_equal(nifti_written$header$bitpix, 32L);
  testthat::expect_equal(nifti_written$header$datatype, 8L);
  testthat::expect_equal(nifti_written$header$pix_dim, rep(0.0, 8L));
  testthat::expect_equal(nifti_written$header$vox_offset, 352);

  header_reread = read.nifti1.header(int_nifti_file);
  testthat::expect_equal(header_reread$dim, c(2, 50, 2, 1, 1, 1, 1, 1));
  testthat::expect_equal(nifti.datadim.from.dimfield(header_reread$dim), c(50, 2));
  testthat::expect_equal(header_reread$intent_p1, 0L);
  testthat::expect_equal(header_reread$intent_p2, 0L);
  testthat::expect_equal(header_reread$intent_p3, 0L);
  testthat::expect_equal(header_reread$pix_dim, rep(0.0, 8L));
  testthat::expect_equal(header_reread$vox_offset, 352);
  testthat::expect_equal(header_reread$cal_max, 100);
  testthat::expect_equal(header_reread$cal_min, 1);
  testthat::expect_equal(header_reread$glmin, 0L);
  testthat::expect_equal(header_reread$glmax, 0L);

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
  testthat::expect_equal(double_data, data_reread, tolerance = 1e-5);
})

