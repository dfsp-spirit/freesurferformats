# Tests for the MRtrix TCK/TSF track readers. The test files are generated on the
# fly by the helpers in helper-functions-for-tests.R, so these tests do not
# depend on any downloaded data.

testthat::test_that('read.dti.tck() reads tracks and metadata', {
  tracks <- make_test_tracks(5L, 3L)
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);
  tck <- freesurferformats::read.dti.tck(fp);

  testthat::expect_type(tck, 'list');
  testthat::expect_named(tck, c('header', 'tracks'));
  testthat::expect_equal(tck$header$id, 'mrtrix tracks');
  testthat::expect_equal(tck$header$count, 5L);
  testthat::expect_equal(tck$header$datatype, 'Float32LE');
  testthat::expect_true(freesurferformats::is.fs.tracts(tck$tracks));

  testthat::expect_equal(length(tck$tracks), 5L);
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(tck$tracks), rep(3L, 5L));
  testthat::expect_equal(nrow(freesurferformats::fs.tracts.coords(tck$tracks)), 15L);

  # Coordinates must survive the float32 round trip.
  for (track_idx in seq_along(tracks)) {
    testthat::expect_equal(tck$tracks[[track_idx]], tracks[[track_idx]], tolerance = 1e-4);
  }
})


testthat::test_that('read.dti.tck() supports all documented datatypes', {
  tracks <- make_test_tracks(3L, 4L);
  for (datatype in c('Float32LE', 'Float32BE', 'Float64LE', 'Float64BE')) {
    fp <- tempfile(fileext = '.tck');
    write_test_tck(fp, tracks, datatype = datatype);
    tck <- freesurferformats::read.dti.tck(fp);
    tolerance <- if (startsWith(datatype, 'Float64')) 1e-12 else 1e-4;
    testthat::expect_equal(tck$tracks[[1]], tracks[[1]], tolerance = tolerance,
                           info = datatype);
  }
})


testthat::test_that('read.dti.tck() accepts small files that used to be rejected', {
  # Files below 100 KB used to be rejected by a heuristic file size check.
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, list(cbind(c(0, 1), c(0, 1), c(0, 1)), cbind(c(2, 3), c(2, 3), c(2, 3))));
  testthat::expect_true(file.size(fp) < 100 * 1024);
  tck <- freesurferformats::read.dti.tck(fp);
  testthat::expect_equal(length(tck$tracks), 2L);
})


testthat::test_that('read.dti.tck() handles headers with more than 50 lines', {
  tracks <- make_test_tracks(4L, 2L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks, extra_header_lines = sprintf('note%02d: padding', 1:80));
  tck <- freesurferformats::read.dti.tck(fp);
  testthat::expect_equal(length(tck$tracks), 4L);
  testthat::expect_equal(tck$header$note01, 'padding');
})


testthat::test_that('read.dti.tck() does not require the optional count header entry', {
  # The MRtrix documentation lists only 'file' and 'datatype' as required.
  tracks <- make_test_tracks(3L, 2L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks, count_key = FALSE);
  tck <- freesurferformats::read.dti.tck(fp);
  testthat::expect_equal(length(tck$tracks), 3L);
})


testthat::test_that('read.dti.tck() accepts both streamline separator layouts', {
  tracks <- make_test_tracks(3L, 2L);
  for (nan_after_last in c(TRUE, FALSE)) {
    fp <- tempfile(fileext = '.tck');
    write_test_tck(fp, tracks, nan_after_last = nan_after_last);
    tck <- freesurferformats::read.dti.tck(fp);
    testthat::expect_equal(length(tck$tracks), 3L, info = sprintf('nan_after_last=%s', nan_after_last));
    testthat::expect_equal(tck$tracks[[3]], tracks[[3]], tolerance = 1e-4,
                           info = sprintf('nan_after_last=%s', nan_after_last));
  }
})


testthat::test_that('read.dti.tck() handles files with and without an end-of-file marker', {
  tracks <- make_test_tracks(3L, 2L);

  # Files without the marker are accepted, as the MRtrix reference reader does.
  fp_no_term <- tempfile(fileext = '.tck');
  write_test_tck(fp_no_term, tracks, terminator = FALSE);
  tck <- freesurferformats::read.dti.tck(fp_no_term);
  testthat::expect_equal(length(tck$tracks), 3L);
  testthat::expect_equal(tck$tracks[[3]], tracks[[3]], tolerance = 1e-4);

  # But a file that stores fewer streamlines than the header states, and has no
  # end-of-file marker either, is reported as possibly truncated.
  fp_short <- tempfile(fileext = '.tck');
  write_test_tck(fp_short, tracks, terminator = FALSE, count_value = 9L);
  testthat::expect_warning(freesurferformats::read.dti.tck(fp_short), 'truncated');
})


testthat::test_that('read.dti.tck() detects files that are not in TCK format', {
  tracks <- make_test_tracks(3L, 2L);

  # Missing 'END' line.
  fp_no_end <- tempfile(fileext = '.tck');
  writeLines(c('mrtrix tracks', 'datatype: Float32LE', 'file: . 50'), fp_no_end);
  testthat::expect_error(freesurferformats::read.dti.tck(fp_no_end), 'END');

  # Unsupported datatype.
  fp_bad_dtype <- tempfile(fileext = '.tck');
  write_test_tck(fp_bad_dtype, tracks, datatype = 'Int16LE');
  testthat::expect_error(freesurferformats::read.dti.tck(fp_bad_dtype), 'datatype');

  # Data offset beyond the end of the file.
  fp_bad_offset <- tempfile(fileext = '.tck');
  writeLines(c('mrtrix tracks', 'count: 2', 'datatype: Float32LE', 'file: . 999999', 'END'),
             fp_bad_offset);
  testthat::expect_error(freesurferformats::read.dti.tck(fp_bad_offset), 'truncated');
})


testthat::test_that('read.dti.tck() reports unusable files clearly', {
  # A 'count: 0' header states that the file stores no streamlines.
  fp_zero <- tempfile(fileext = '.tck');
  write_test_tck(fp_zero, list(cbind(c(0, 1), c(0, 1), c(0, 1))), count_value = 0L);
  testthat::expect_error(freesurferformats::read.dti.tck(fp_zero), 'any streamlines');

  fp_not_tck <- tempfile(fileext = '.tck');
  writeLines(rep('this is not a tck file', 5L), fp_not_tck);
  testthat::expect_error(freesurferformats::read.dti.tck(fp_not_tck), 'not in TCK/TSF');

  # Multi-file TCK files are not supported.
  fp_multifile <- tempfile(fileext = '.tck');
  writeLines(c('mrtrix tracks', 'count: 1', 'datatype: Float32LE', 'file: other.tck 100', 'END'),
             fp_multifile);
  testthat::expect_error(freesurferformats::read.dti.tck(fp_multifile), 'Multi-file');
})


testthat::test_that('read.dti.tck() reads gzip-compressed files', {
  tracks <- make_test_tracks(6L, 4L);
  fp_plain <- tempfile(fileext = '.tck');
  write_test_tck(fp_plain, tracks);

  fp_gz <- tempfile(fileext = '.tck.gz');
  gz_con <- gzfile(fp_gz, 'wb');
  writeBin(readBin(fp_plain, 'raw', file.size(fp_plain)), gz_con);
  close(gz_con);

  tck_plain <- freesurferformats::read.dti.tck(fp_plain);
  tck_gz <- freesurferformats::read.dti.tck(fp_gz);

  testthat::expect_equal(length(tck_gz$tracks), length(tck_plain$tracks));
  testthat::expect_equal(freesurferformats::fs.tracts.coords(tck_gz$tracks),
                         freesurferformats::fs.tracts.coords(tck_plain$tracks));
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(tck_gz$tracks),
                         freesurferformats::fs.tracts.lengths(tck_plain$tracks));
  testthat::expect_true(tck_gz$header$derived$gzipped);
  testthat::expect_false(tck_plain$header$derived$gzipped);
})


testthat::test_that('read.dti.tck() detects gzip from the content, not the file name', {
  tracks <- make_test_tracks(4L, 3L);
  fp_plain <- tempfile(fileext = '.tck');
  write_test_tck(fp_plain, tracks);

  # A gzip-compressed file that does not look compressed by its name.
  fp_disguised <- tempfile(fileext = '.tck');
  gz_con <- gzfile(fp_disguised, 'wb');
  writeBin(readBin(fp_plain, 'raw', file.size(fp_plain)), gz_con);
  close(gz_con);

  tck <- freesurferformats::read.dti.tck(fp_disguised);
  testthat::expect_equal(length(tck$tracks), 4L);
  testthat::expect_equal(tck$tracks[[1]], tracks[[1]], tolerance = 1e-4);
})


testthat::test_that('read.dti.tck() can read only the first N tracks', {
  tracks <- make_test_tracks(20L, 5L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);

  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, max_tracks = 3L)$tracks), 3L);
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, max_tracks = 1L)$tracks), 1L);
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, max_tracks = 20L)$tracks), 20L);

  # A subset read has to return exactly the same first tracks as a full read.
  full <- freesurferformats::read.dti.tck(fp);
  part <- freesurferformats::read.dti.tck(fp, max_tracks = 3L);
  testthat::expect_equal(part$tracks[[1]], full$tracks[[1]]);
  testthat::expect_equal(part$tracks[[3]], full$tracks[[3]]);

  # Works for gzip-compressed input as well, where reading stops early.
  fp_gz <- tempfile(fileext = '.tck.gz');
  gz_con <- gzfile(fp_gz, 'wb');
  writeBin(readBin(fp, 'raw', file.size(fp)), gz_con);
  close(gz_con);
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp_gz, max_tracks = 4L)$tracks), 4L);
})


testthat::test_that('the fs.tracts container behaves like a list of tracks', {
  tracks <- make_test_tracks(8L, 3L, seed = 42L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);
  tr <- freesurferformats::read.dti.tck(fp)$tracks;

  testthat::expect_true(freesurferformats::is.fs.tracts(tr));
  testthat::expect_equal(length(tr), 8L);

  # Element access.
  testthat::expect_equal(tr[[2]], tracks[[2]], tolerance = 1e-4);
  testthat::expect_equal(tr[[8]], tracks[[8]], tolerance = 1e-4);
  testthat::expect_error(tr[[9]], 'out of range');
  testthat::expect_error(tr[[0]], 'out of range');

  # Subsetting.
  sub <- tr[c(1, 5)];
  testthat::expect_true(freesurferformats::is.fs.tracts(sub));
  testthat::expect_equal(length(sub), 2L);
  testthat::expect_equal(sub[[2]], tracks[[5]], tolerance = 1e-4);

  # Conversion to a plain list, which is what lapply() uses internally.
  as_list <- as.list(tr);
  testthat::expect_type(as_list, 'list');
  testthat::expect_equal(length(as_list), 8L);
  testthat::expect_equal(as_list[[3]], tracks[[3]], tolerance = 1e-4);
  testthat::expect_equal(unlist(lapply(tr, nrow)), rep(3L, 8L));

  # Compact accessors.
  testthat::expect_equal(nrow(freesurferformats::fs.tracts.coords(tr)), 24L);
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(tr), rep(3L, 8L));
  testthat::expect_error(freesurferformats::fs.tracts.coords('not tracts'), 'fs.tracts');
})


testthat::test_that('read.dti.tsf() reads track scalars', {
  track_values <- list(c(1.5, 2.5, 3.5), c(4, 5), 6);
  fp <- tempfile(fileext = '.tsf');
  write_test_tsf(fp, track_values);
  tsf <- freesurferformats::read.dti.tsf(fp);

  testthat::expect_type(tsf, 'list');
  testthat::expect_named(tsf, c('header', 'scalars'));
  testthat::expect_equal(tsf$header$id, 'mrtrix track scalars');
  testthat::expect_equal(tsf$scalars$merged, c(1.5, 2.5, 3.5, 4, 5, 6));
  testthat::expect_equal(tsf$scalars$lengths, c(3L, 2L, 1L));
  testthat::expect_equal(length(tsf$scalars$scalar_list), 3L);
  testthat::expect_equal(tsf$scalars$scalar_list[[1]], c(1.5, 2.5, 3.5), tolerance = 1e-6);
  testthat::expect_equal(tsf$scalars$scalar_list[[3]], 6, tolerance = 1e-6);
})


testthat::test_that('read.dti.tsf() reads gzip-compressed files', {
  track_values <- list(c(1, 2, 3), c(4, 5));
  fp_plain <- tempfile(fileext = '.tsf');
  write_test_tsf(fp_plain, track_values);

  fp_gz <- tempfile(fileext = '.tsf.gz');
  gz_con <- gzfile(fp_gz, 'wb');
  writeBin(readBin(fp_plain, 'raw', file.size(fp_plain)), gz_con);
  close(gz_con);

  tsf <- freesurferformats::read.dti.tsf(fp_gz);
  testthat::expect_equal(tsf$scalars$merged, c(1, 2, 3, 4, 5));
  testthat::expect_equal(tsf$scalars$lengths, c(3L, 2L));
})


testthat::test_that('read.dti.tck() and read.dti.tsf() load the optional test data', {
  testthat::skip_on_cran();
  freesurferformats::download_opt_data();
  dwi_dir <- freesurferformats::get_opt_data_filepath('dwi');

  fp_tck <- file.path(dwi_dir, 'tracks.tck');
  testthat::skip_if_not(file.exists(fp_tck));
  TCK <- freesurferformats::read.dti.tck(fp_tck);

  testthat::expect_type(TCK, 'list');
  testthat::expect_named(TCK, c('header', 'tracks'));
  testthat::expect_equal(TCK$header$id, 'mrtrix tracks');
  testthat::expect_true(TCK$header$count > 0);
  testthat::expect_true(freesurferformats::is.fs.tracts(TCK$tracks));
  testthat::expect_true(length(TCK$tracks) > 0L);
  testthat::expect_true(all(is.finite(freesurferformats::fs.tracts.coords(TCK$tracks))));
  testthat::expect_equal(sum(freesurferformats::fs.tracts.lengths(TCK$tracks)),
                         nrow(freesurferformats::fs.tracts.coords(TCK$tracks)));

  fp_tsf <- file.path(dwi_dir, 'tracks.tsf');
  testthat::skip_if_not(file.exists(fp_tsf));
  TSF <- freesurferformats::read.dti.tsf(fp_tsf);

  testthat::expect_type(TSF, 'list');
  testthat::expect_named(TSF, c('header', 'scalars'));
  testthat::expect_equal(TSF$header$id, 'mrtrix track scalars');
  testthat::expect_true(TSF$header$count > 0);
  testthat::expect_true(is.numeric(TSF$scalars$merged));
  testthat::expect_true(all(is.finite(TSF$scalars$merged)));
  testthat::expect_true(is.list(TSF$scalars$scalar_list));
  testthat::expect_equal(length(TSF$scalars$merged), sum(lengths(TSF$scalars$scalar_list)));
})
