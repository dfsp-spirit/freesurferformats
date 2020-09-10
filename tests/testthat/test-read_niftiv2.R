

test_that("The header of NIFTI v2 files can be read using nifti2.header.", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

  freesurferformats::download_opt_data();
  data_dir = freesurferformats::get_opt_data_filepath("nifti2");
  nii_v2_file = file.path(data_dir, 'avg152T1_LR_nifti2.nii.gz');

  hdr = nifti2.header(nii_v2_file);

  testthat::expect_true(is.list(hdr));
  testthat::expect_equal(hdr$description, 'FSL3.2beta');
  testthat::expect_equal(hdr$cal_max, 255); # max color display value (display range definition)
  testthat::expect_equal(hdr$cal_min, 0);   # min color display value
  testthat::expect_equal(hdr$vox_offset, 544L);   # start of data (offset in bytes)
  testthat::expect_equal(hdr$dim, c(3L, 91L, 109L, 91L, 1L, 1L, 1L, 1L));   # dimensions. first entry is number of used dimensions (num_dim). then follow dim sizes. the ones after num_dim + 1 should be ignored.
  testthat::expect_equal(hdr$bitpix, 32L);   # number of bytes per value (in the data part of the file)
  testthat::expect_equal(hdr$datatype, 16L);   # the NIFTI data type (defines whether this is integer, float, etc). See NIFTI2 spec.
})


test_that("Data from NIFTI v2 files can be read.", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

  freesurferformats::download_opt_data();
  data_dir = freesurferformats::get_opt_data_filepath("nifti2");
  nii_v2_file = file.path(data_dir, 'avg152T1_LR_nifti2.nii.gz');

  hdr = nifti2.header(nii_v2_file);
  niidata = nifti2.data(nii_v2_file);

  testthat::expect_equal(dim(niidata), c(91, 109, 91));
})
