test_that("NIFTI v1 files with FreeSurfer hack can be read.", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");

  morph_file_curv = file.path(subjects_dir, "subject1", "surf", "lh.thickness");
  morph_file_nii = file.path(subjects_dir, "subject1", "surf", "lh.thickness.nii.gz");

  morph_data_curv = read.fs.morph(morph_file_curv);
  morph_data_nii = nifti1.data(morph_file_nii);

  testthat::expect_equal(morph_data_curv, morph_data_nii);
})


test_that("NIFTI v1 files with FreeSurfer hack can be read using read.fs.morph.", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");

  morph_file_curv = file.path(subjects_dir, "subject1", "surf", "lh.thickness");
  morph_file_nii = file.path(subjects_dir, "subject1", "surf", "lh.thickness.nii.gz");

  morph_data_curv = read.fs.morph(morph_file_curv);
  morph_data_nii = read.fs.morph(morph_file_nii);

  testthat::expect_equal(morph_data_curv, morph_data_nii);
  testthat::expect_true(nifti.file.uses.fshack(morph_file_nii));
})


test_that("NIFTI v1 file headers with FreeSurfer hack can be read.", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");

  morph_file_nii = file.path(subjects_dir, "subject1", "surf", "lh.thickness.nii.gz");

  nh = nifti1.header(morph_file_nii);
  testthat::expect_equal(nh$endian, 'little');
  testthat::expect_equal(nh$glmin, 149244L);
  testthat::expect_equal(nh$vox_offset, 352L);
  testthat::expect_equal(nh$dim_raw, c(3, -1, 1, 1, 1, 1, 1, 1));
  testthat::expect_equal(nh$dim, c(3, 149244, 1, 1, 1, 1, 1, 1));
})






