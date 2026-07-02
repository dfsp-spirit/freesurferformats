testthat::test_that('read.dti.track() loads valid files in TSF und TCK formats', {
  testthat::skip_on_cran();
  freesurferformats::download_opt_data();
  dwi_dir = freesurferformats::get_opt_data_filepath("dwi");

  # TCK
  fp_tck = file.path(dwi_dir, 'tracks.tck');
  testthat::skip_if_not(file.exists(fp_tck));
  TCK = freesurferformats::read.dti.tck(fp_tck);

  testthat::expect_type(TCK, 'list');
  expect_named(TCK, c('header', 'tracks'));

  testthat::expect_all_equal(TCK$header$id, 'mrtrix tracks');
  testthat::expect_all_true(TCK$header$count > 0);

  testthat::expect_all_true(is.list(TCK$tracks));
  testthat::expect_all_equal(length(TCK$tracks), TCK$header$max_num_tracks);
  testthat::expect_all_true(is.finite(unlist(TCK$tracks, recursive=FALSE, use.names=FALSE)));

  # TSF
  fp_tsf = file.path(dwi_dir, 'tracks.tsf');
  testthat::skip_if_not(file.exists(fp_tsf));
  TSF = freesurferformats::read.dti.tsf(fp_tsf);

  testthat::expect_type(TSF, 'list');
  testthat::expect_named(TSF, c('header', 'scalars'));

  testthat::expect_all_equal(TSF$header$id, 'mrtrix track scalars');
  testthat::expect_all_true(TSF$header$count > 0);

  testthat::expect_all_true(is.numeric(TSF$scalars$merged));
  testthat::expect_all_true(is.finite(TSF$scalars$merged));

  testthat::expect_all_true(is.list(TSF$scalars$scalar_list));
  testthat::expect_all_equal(length(TSF$scalars$merged), sum(lengths(TSF$scalars$scalar_list)));
})
