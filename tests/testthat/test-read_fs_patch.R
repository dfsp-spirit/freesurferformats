

test_that("A FreeSurfer binary patch file can be read using read.fs.patch", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  testthat::skip_on_cran(); # cannot download testdata on CRAN.
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  patch_file = file.path(subjects_dir, "subject1", 'ext', 'lh.cortex.patch.3d');
  skip_if_not(file.exists(patch_file), message="Test data missing.") ;

  fspatch = read.fs.patch(patch_file);
  patch_data = fspatch$vertices;

  expect_true(is.matrix(patch_data));
  expect_equal(ncol(patch_data), 7);
  expect_equal(nrow(patch_data), 149297);

  # Test indices:
  expect_equal(min(patch_data[,1]), 1);   # smallest one-based index must be 1
  expect_equal(min(patch_data[,7]), 0);   # smallest zero-based index must be 0

  expect_error(read.fs.patch(patch_file, format = 'no such format')); # invalid format
})


test_that("A FreeSurfer ASCII patch file can be read using read.fs.patch", {
  testthat::skip_on_cran(); # cannot download testdata on CRAN.
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  patch_file = file.path(subjects_dir, "subject1", 'ext', 'lh.cortex.patch.3d.asc');
  skip_if_not(file.exists(patch_file), message="Test data missing.") ;

  fspatch = read.fs.patch(patch_file);
  patch_data = fspatch$vertices;

  expect_true(is.matrix(patch_data));
  expect_equal(ncol(patch_data), 7);
  expect_equal(nrow(patch_data), 149297);

  # Test indices:
  expect_equal(min(patch_data[,1]), 1);   # smallest one-based index must be 1
  expect_equal(min(patch_data[,7]), 0);   # smallest zero-based index must be 0

  expect_error(read.fs.patch(patch_file, format = 'no such format')); # invalid format
})

