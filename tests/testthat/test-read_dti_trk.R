# Tests for the TrackVis TRK reader. The test files are generated on the fly by
# the helpers in helper-functions-for-tests.R, so these tests do not depend on
# any downloaded data.

testthat::test_that('read.dti.trk() reads tracks and header fields', {
  tracks <- make_test_tracks(5L, 4L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks);
  trk <- freesurferformats::read.dti.trk(fp);

  testthat::expect_type(trk, 'list');
  testthat::expect_named(trk, c('header', 'tracks'));
  testthat::expect_equal(trk$header$id_string, 'TRACK');
  testthat::expect_equal(trk$header$version, 2L);
  testthat::expect_equal(trk$header$hdr_size, 1000L);
  testthat::expect_equal(trk$header$n_count, 5L);
  testthat::expect_equal(trk$header$voxel_order, 'LPS');
  testthat::expect_equal(trk$header$voxel_size, c(1, 1, 1));

  testthat::expect_true(freesurferformats::is.fs.tracts(trk$tracks));
  testthat::expect_equal(length(trk$tracks), 5L);
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(trk$tracks), rep(4L, 5L));
  testthat::expect_equal(nrow(freesurferformats::fs.tracts.coords(trk$tracks)), 20L);

  for (track_idx in seq_along(tracks)) {
    testthat::expect_equal(trk$tracks[[track_idx]]$coords, tracks[[track_idx]], tolerance = 1e-4);
    testthat::expect_equal(trk$tracks[[track_idx]]$num_points, 4L);
    testthat::expect_null(trk$tracks[[track_idx]]$scalars);
    testthat::expect_null(trk$tracks[[track_idx]]$properties);
  }
})


testthat::test_that('read.dti.trk() returns the documented per-track structure', {
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, make_test_tracks(2L, 3L));
  track <- freesurferformats::read.dti.trk(fp)$tracks[[1]];

  testthat::expect_named(track, c('scalars', 'properties', 'coords', 'num_points'));
  testthat::expect_true(is.matrix(track$coords));
  testthat::expect_equal(ncol(track$coords), 3L);
})


testthat::test_that('read.dti.trk() reads per-point scalars and per-track properties', {
  tracks <- make_test_tracks(3L, 4L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, n_scalars = 2L, n_properties = 3L);
  trk <- freesurferformats::read.dti.trk(fp);

  testthat::expect_equal(trk$header$n_scalars, 2L);
  testthat::expect_equal(trk$header$n_properties, 3L);

  track <- trk$tracks[[1]];
  testthat::expect_equal(dim(track$coords), c(4L, 3L));
  testthat::expect_equal(dim(track$scalars), c(4L, 2L));
  testthat::expect_equal(track$properties, c(10001, 10002, 10003), tolerance = 1e-4);

  # The writer stores scalars as '1000 * scalar_index + point_index'.
  testthat::expect_equal(track$scalars[1, ], c(1001, 2001), tolerance = 1e-4);
  testthat::expect_equal(track$scalars[4, ], c(1004, 2004), tolerance = 1e-4);

  # The third track has its own property values.
  testthat::expect_equal(trk$tracks[[3]]$properties, c(30001, 30002, 30003), tolerance = 1e-4);

  # Coordinates are still read correctly when scalars are interleaved.
  testthat::expect_equal(track$coords, tracks[[1]], tolerance = 1e-4);
})


testthat::test_that('read.dti.trk() reads files with an unknown track count to the end', {
  tracks <- make_test_tracks(4L, 3L);

  # 'n_count' of 0 means the number of tracks was not stored.
  fp_zero <- tempfile(fileext = '.trk');
  write_test_trk(fp_zero, tracks, n_count = 0L);
  trk <- freesurferformats::read.dti.trk(fp_zero);
  testthat::expect_equal(trk$header$n_count, 0L);
  testthat::expect_equal(length(trk$tracks), 4L);
  testthat::expect_equal(trk$tracks[[4]]$coords, tracks[[4]], tolerance = 1e-4);

  # A stored count that is too small is respected, a count that is too large is
  # reported as truncation.
  fp_small <- tempfile(fileext = '.trk');
  write_test_trk(fp_small, tracks, n_count = 2L);
  testthat::expect_equal(length(freesurferformats::read.dti.trk(fp_small)$tracks), 2L);

  fp_large <- tempfile(fileext = '.trk');
  write_test_trk(fp_large, tracks, n_count = 9L);
  testthat::expect_error(freesurferformats::read.dti.trk(fp_large), 'Truncated');
})


testthat::test_that('read.dti.trk() detects truncated files', {
  tracks <- make_test_tracks(3L, 5L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks);

  raw <- readBin(fp, 'raw', file.size(fp));
  fp_trunc <- tempfile(fileext = '.trk');
  writeBin(raw[1:(length(raw) - 30L)], fp_trunc);
  testthat::expect_error(freesurferformats::read.dti.trk(fp_trunc), 'Truncated');
})


testthat::test_that('read.dti.trk() supports big endian files', {
  tracks <- make_test_tracks(3L, 3L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, endian = 'big');
  trk <- freesurferformats::read.dti.trk(fp);

  testthat::expect_equal(trk$header$hdr_size, 1000L);
  testthat::expect_equal(length(trk$tracks), 3L);
  testthat::expect_equal(trk$tracks[[2]]$coords, tracks[[2]], tolerance = 1e-4);
})


testthat::test_that('read.dti.trk() can read only the first N tracks', {
  tracks <- make_test_tracks(10L, 3L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks);

  full <- freesurferformats::read.dti.trk(fp);
  part <- freesurferformats::read.dti.trk(fp, max_tracks = 3L);

  testthat::expect_equal(length(part$tracks), 3L);
  testthat::expect_equal(part$tracks[[1]]$coords, full$tracks[[1]]$coords);
  testthat::expect_equal(part$tracks[[3]]$coords, full$tracks[[3]]$coords);

  # Subset reading must also work when the count is unknown.
  fp_zero <- tempfile(fileext = '.trk');
  write_test_trk(fp_zero, tracks, n_count = 0L);
  testthat::expect_equal(length(freesurferformats::read.dti.trk(fp_zero, max_tracks = 2L)$tracks), 2L);
})


testthat::test_that('read.dti.trk() exposes the vox2ras matrices without applying them', {
  vox2ras <- rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1));
  tracks <- list(cbind(c(85.2, 125.2), c(0, 0), c(0, 0)));
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, vox2ras = vox2ras);

  trk <- freesurferformats::read.dti.trk(fp, shift_origin = TRUE, coords = 'native');
  testthat::expect_equal(trk$header$vox2ras, vox2ras, tolerance = 1e-5);
  testthat::expect_true(!is.null(trk$header$vox2ras_corrected));

  # The coordinates are returned as stored in the file, they are NOT transformed
  # to RAS space. Callers have to apply the matrix themselves.
  testthat::expect_equal(trk$tracks[[1]]$coords[, 1], c(85.2, 125.2), tolerance = 1e-4);

  # Not passing 'coords' at all warns about exactly that.
  testthat::expect_warning(freesurferformats::read.dti.trk(fp), 'not in RAS space');

  trk_no_shift <- freesurferformats::read.dti.trk(fp, shift_origin = FALSE, coords = 'native');
  testthat::expect_null(trk_no_shift$header$vox2ras_corrected);
})


testthat::test_that('the fs.tracts container handles TRK scalars and properties', {
  tracks <- make_test_tracks(5L, 3L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, n_scalars = 1L, n_properties = 2L);
  tr <- freesurferformats::read.dti.trk(fp)$tracks;

  testthat::expect_true(freesurferformats::is.fs.tracts(tr));
  testthat::expect_equal(length(tr), 5L);

  # Subsetting has to keep scalars and properties aligned with their tracks.
  sub <- tr[c(2, 4)];
  testthat::expect_equal(length(sub), 2L);
  testthat::expect_equal(sub[[1]]$coords, tracks[[2]], tolerance = 1e-4);
  testthat::expect_equal(sub[[2]]$coords, tracks[[4]], tolerance = 1e-4);
  testthat::expect_equal(sub[[1]]$properties, c(20001, 20002), tolerance = 1e-4);
  testthat::expect_equal(sub[[2]]$properties, c(40001, 40002), tolerance = 1e-4);
  testthat::expect_equal(dim(sub[[1]]$scalars), c(3L, 1L));

  # as.list() and lapply() have to work for TRK data as well.
  as_list <- as.list(tr);
  testthat::expect_equal(as_list[[5]]$properties, c(50001, 50002), tolerance = 1e-4);
  testthat::expect_equal(unlist(lapply(tr, function(track) track$num_points)), rep(3L, 5L));
})


testthat::test_that('read.dti.trk() rejects files that are not TRK', {
  fp <- tempfile(fileext = '.trk');
  writeLines(rep('this is not a trk file at all', 3L), fp);
  testthat::expect_error(freesurferformats::read.dti.trk(fp), 'not in TRK format');
})
