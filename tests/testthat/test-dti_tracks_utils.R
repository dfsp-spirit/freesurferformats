# Tests for the file-level tract utilities (header readers, subsetting, the
# streaming iterator and the aggregate counters) and for the TCK/TRK writers.

testthat::test_that('the TCK/TSF header readers read only the header', {
  tracks <- make_test_tracks(4L, 3L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks, extra_header_lines = 'method: iFOD2');

  header <- freesurferformats::read.dti.tck.header(fp);
  full <- freesurferformats::read.dti.tck(fp);

  testthat::expect_equal(header$id, 'mrtrix tracks');
  testthat::expect_equal(header$count, 4L);
  testthat::expect_equal(header$method, 'iFOD2');
  testthat::expect_true(header$derived$data_offset > 0);
  testthat::expect_equal(header$derived$endian, 'little');
  testthat::expect_equal(header$derived$dsize, 4L);
  testthat::expect_false(header$derived$gzipped);
  testthat::expect_equal(header, full$header);

  # The TSF reader returns the same header.
  fp_tsf <- tempfile(fileext = '.tsf');
  write_test_tsf(fp_tsf, list(c(1, 2), c(3)));
  header_tsf <- freesurferformats::read.dti.tsf.header(fp_tsf);
  testthat::expect_equal(header_tsf$id, 'mrtrix track scalars');
  testthat::expect_equal(header_tsf$count, 2L);

  # A gzip-compressed file is reported as such.
  fp_gz <- tempfile(fileext = '.tck.gz');
  gz_con <- gzfile(fp_gz, 'wb');
  writeBin(readBin(fp, 'raw', file.size(fp)), gz_con);
  close(gz_con);
  testthat::expect_true(freesurferformats::read.dti.tck.header(fp_gz)$derived$gzipped);

  # The header reader reports broken files, too.
  fp_not_tck <- tempfile(fileext = '.tck');
  writeLines(rep('not a tck file', 4L), fp_not_tck);
  testthat::expect_error(freesurferformats::read.dti.tck.header(fp_not_tck), 'not in TCK/TSF');
})


testthat::test_that('the TRK header reader reads only the header', {
  tracks <- make_test_tracks(3L, 4L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, voxel_size = c(2, 2, 2), voxel_order = 'RAS', n_count = 3L);

  header <- freesurferformats::read.dti.trk.header(fp);
  full <- freesurferformats::read.dti.trk(fp);

  testthat::expect_equal(header$id_string, 'TRACK');
  testthat::expect_equal(header$version, 2L);
  testthat::expect_equal(header$hdr_size, 1000L);
  testthat::expect_equal(header$n_count, 3L);
  testthat::expect_equal(header$voxel_size, c(2, 2, 2));
  testthat::expect_equal(header$voxel_order, 'RAS');
  testthat::expect_true(!is.null(header$vox2ras_corrected));
  testthat::expect_null(freesurferformats::read.dti.trk.header(fp, shift_origin = FALSE)$vox2ras_corrected);
  # read.dti.trk() adds the coordinate system of the tracks it returns.
  testthat::expect_equal(header, full$header[setdiff(names(full$header), 'coords_space')]);
})


testthat::test_that('read.dti.tck() can skip tracks', {
  tracks <- make_test_tracks(10L, 3L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);

  skipped <- freesurferformats::read.dti.tck(fp, skip_tracks = 4L);
  testthat::expect_equal(length(skipped$tracks), 6L);
  testthat::expect_equal(skipped$tracks[[1]], tracks[[5]], tolerance = 1e-4);

  # Skipping and limiting combine.
  window <- freesurferformats::read.dti.tck(fp, skip_tracks = 4L, max_tracks = 2L);
  testthat::expect_equal(length(window$tracks), 2L);
  testthat::expect_equal(window$tracks[[1]], tracks[[5]], tolerance = 1e-4);
  testthat::expect_equal(window$tracks[[2]], tracks[[6]], tolerance = 1e-4);

  # Skipping everything yields nothing, and skipping more than exists is not an error.
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, skip_tracks = 10L)$tracks), 0L);
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, skip_tracks = 50L)$tracks), 0L);
})


testthat::test_that('read.dti.tck() can filter tracks by a bounding box', {
  tracks <- list(
    cbind(c(1, 1), c(1, 1), c(1, 1)),   # inside
    cbind(c(50, 51), c(50, 51), c(50, 51)), # outside
    cbind(c(1, 50), c(3, 3), c(3, 3))   # partly inside
  );
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);

  inside <- freesurferformats::read.dti.tck(fp, bbox = c(0, 10, 0, 10, 0, 10));
  testthat::expect_equal(length(inside$tracks), 2L);
  testthat::expect_equal(inside$tracks[[1]], tracks[[1]], tolerance = 1e-4);
  testthat::expect_equal(inside$tracks[[2]], tracks[[3]], tolerance = 1e-4);

  # An empty box selects nothing, a huge box selects everything.
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, bbox = c(0, 0.5, 0, 0.5, 0, 0.5))$tracks), 0L);
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp, bbox = c(-100, 100, -100, 100, -100, 100))$tracks), 3L);

  # The box is validated.
  testthat::expect_error(freesurferformats::read.dti.tck(fp, bbox = c(1, 2, 3)), 'length 6');
  testthat::expect_error(freesurferformats::read.dti.tck(fp, bbox = c(5, 1, 0, 1, 0, 1)), 'minimum');
})


testthat::test_that('read.dti.trk() can skip and filter tracks', {
  tracks <- make_test_tracks(6L, 3L);
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks);

  skipped <- freesurferformats::read.dti.trk(fp, skip_tracks = 2L);
  testthat::expect_equal(length(skipped$tracks), 4L);
  testthat::expect_equal(skipped$tracks[[1]]$coords, tracks[[3]], tolerance = 1e-4);

  # A box that contains everything selects everything.
  complete_bbox <- c(min(unlist(lapply(tracks, function(t) t[, 1]))), max(unlist(lapply(tracks, function(t) t[, 1]))),
                     min(unlist(lapply(tracks, function(t) t[, 2]))), max(unlist(lapply(tracks, function(t) t[, 2]))),
                     min(unlist(lapply(tracks, function(t) t[, 3]))), max(unlist(lapply(tracks, function(t) t[, 3]))));
  testthat::expect_equal(length(freesurferformats::read.dti.trk(fp, bbox = complete_bbox)$tracks), 6L);

  # Selecting a single track by a box around its first point. The box needs a
  # little slack, since the coordinates are stored as 4 byte floats.
  one <- tracks[[4]];
  tight_bbox <- c(one[1, 1] - 0.1, one[1, 1] + 0.1, one[1, 2] - 0.1, one[1, 2] + 0.1,
                  one[1, 3] - 0.1, one[1, 3] + 0.1);
  selected <- freesurferformats::read.dti.trk(fp, bbox = tight_bbox);
  testthat::expect_true(length(selected$tracks) >= 1L);
  testthat::expect_equal(selected$tracks[[1]]$coords[1, ], one[1, ], tolerance = 1e-4);
})


testthat::test_that('dti.track.count() and dti.track.bbox() stream through a file', {
  tracks <- make_test_tracks(7L, 4L);
  fp_tck <- tempfile(fileext = '.tck');
  write_test_tck(fp_tck, tracks);
  fp_trk <- tempfile(fileext = '.trk');
  write_test_trk(fp_trk, tracks);

  all_coords <- do.call(rbind, tracks);
  expected_bbox <- c(min(all_coords[, 1]), max(all_coords[, 1]),
                     min(all_coords[, 2]), max(all_coords[, 2]),
                     min(all_coords[, 3]), max(all_coords[, 3]));

  for (fp in c(fp_tck, fp_trk)) {
    testthat::expect_equal(freesurferformats::dti.track.count(fp), 7L);
    testthat::expect_equal(freesurferformats::dti.track.bbox(fp), expected_bbox, tolerance = 1e-4);
  }

  # The count ignores the header, which may be wrong.
  fp_wrong <- tempfile(fileext = '.trk');
  write_test_trk(fp_wrong, tracks, n_count = 99L);
  testthat::expect_equal(freesurferformats::dti.track.count(fp_wrong), 7L);

  fp_not_tracts <- tempfile(fileext = '.txt');
  writeLines('nothing to see here', fp_not_tracts);
  testthat::expect_error(freesurferformats::dti.track.count(fp_not_tracts), 'not in TRK, TCK or TSF');

  # TSF files have no coordinates.
  fp_tsf <- tempfile(fileext = '.tsf');
  write_test_tsf(fp_tsf, list(c(1, 2), c(3)));
  testthat::expect_equal(freesurferformats::dti.track.count(fp_tsf), 2L);
  testthat::expect_error(freesurferformats::dti.track.bbox(fp_tsf), 'no bounding box');
})


testthat::test_that('dti.track.iterator() streams the tracks of a file', {
  tracks <- make_test_tracks(5L, 4L);
  fp_tck <- tempfile(fileext = '.tck');
  write_test_tck(fp_tck, tracks);
  fp_trk <- tempfile(fileext = '.trk');
  write_test_trk(fp_trk, tracks);

  # TCK: the iterator returns n x 3 matrices.
  itr <- freesurferformats::dti.track.iterator(fp_tck);
  collected <- list();
  while (!is.null(track <- itr$next.track())) {
    collected[[length(collected) + 1L]] <- track;
  }
  testthat::expect_equal(length(collected), 5L);
  testthat::expect_equal(itr$tracks.read, 5L);
  testthat::expect_equal(itr$format, 'tck');
  for (track_idx in seq_along(tracks)) {
    testthat::expect_equal(collected[[track_idx]], tracks[[track_idx]], tolerance = 1e-4);
  }
  # Once exhausted, it keeps returning NULL, and closing twice is fine.
  testthat::expect_null(itr$next.track());
  itr$close();
  itr$close();
  testthat::expect_null(itr$next.track());

  # TRK: the iterator returns the same structure as tracks[[i]].
  itr <- freesurferformats::dti.track.iterator(fp_trk, skip_tracks = 1L);
  track <- itr$next.track();
  testthat::expect_named(track, c('scalars', 'properties', 'coords', 'num_points'));
  testthat::expect_equal(track$coords, tracks[[2]], tolerance = 1e-4);
  itr$close();
})


testthat::test_that('dti.track.iterator() supports TSF files', {
  track_values <- list(c(1, 2, 3), c(4, 5), 6);
  fp <- tempfile(fileext = '.tsf');
  write_test_tsf(fp, track_values);

  itr <- freesurferformats::dti.track.iterator(fp);
  testthat::expect_equal(itr$format, 'tsf');
  first <- itr$next.track();
  testthat::expect_true(is.numeric(first));
  testthat::expect_equal(first, c(1, 2, 3), tolerance = 1e-6);
  testthat::expect_equal(itr$next.track(), c(4, 5), tolerance = 1e-6);
  testthat::expect_equal(itr$next.track(), 6, tolerance = 1e-6);
  testthat::expect_null(itr$next.track());
  itr$close();

  testthat::expect_error(freesurferformats::dti.track.iterator(fp, bbox = c(0, 1, 0, 1, 0, 1)), 'bbox');
})


testthat::test_that('read.dti.trk() returns native coordinates by default and warns', {
  vox2ras <- rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1));
  tracks <- list(cbind(c(85.2, 125.2), c(0, 0), c(0, 0)));
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, vox2ras = vox2ras, voxel_order = 'LPS');

  # The coordinates are returned as stored, but the user is warned that they are
  # not in RAS space.
  testthat::expect_warning(
    trk <- freesurferformats::read.dti.trk(fp),
    'not in RAS space'
  );
  testthat::expect_equal(freesurferformats::fs.tracts.coords(trk$tracks)[, 1], c(85.2, 125.2), tolerance = 1e-4);
  testthat::expect_equal(trk$header$coords_space, 'native');

  # Passing 'native' explicitly silences the warning.
  testthat::expect_silent(trk_native <- freesurferformats::read.dti.trk(fp, coords = 'native'));
  testthat::expect_equal(freesurferformats::fs.tracts.coords(trk_native$tracks), freesurferformats::fs.tracts.coords(trk$tracks));

  # A file whose matrix is the identity does not warn, and its coordinates are
  # already in RAS space up to the half-voxel convention.
  fp_identity <- tempfile(fileext = '.trk');
  write_test_trk(fp_identity, tracks);
  testthat::expect_silent(freesurferformats::read.dti.trk(fp_identity));
})


testthat::test_that('read.dti.trk(coords = "ras") applies the TrackVis affine', {
  vox2ras <- rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1));
  tracks <- list(cbind(c(85.2, 125.2), c(0, 0), c(0, 0)));
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, vox2ras = vox2ras, voxel_order = 'LPS', voxel_size = c(1, 1, 1));

  trk_ras <- freesurferformats::read.dti.trk(fp, coords = 'ras');
  ras_x <- freesurferformats::fs.tracts.coords(trk_ras$tracks)[, 1];

  # The stored x values are positive, the RAS values must be negative (left
  # hemisphere), which is the mirroring that used to go unnoticed.
  testthat::expect_true(all(ras_x < 0));
  testthat::expect_equal(ras_x, c(-85.2, -125.2) + 78.5, tolerance = 1e-3);

  # The transformation is exactly nibabel's affine for files whose voxel order
  # agrees with the orientation implied by the matrix.
  testthat::expect_equal(freesurferformats:::trackvis.affine.to.rasmm(trk_ras$header),
                         trk_ras$header$vox2ras_corrected, tolerance = 1e-5);
  testthat::expect_equal(trk_ras$header$coords_space, 'ras');

  # shift_origin = FALSE uses the matrix as stored, without the half-voxel shift.
  trk_dsi <- freesurferformats::read.dti.trk(fp, coords = 'ras', shift_origin = FALSE);
  testthat::expect_equal(freesurferformats::fs.tracts.coords(trk_dsi$tracks)[, 1],
                         c(-85.2, -125.2) + 78.0, tolerance = 1e-3);
})


testthat::test_that('read.dti.trk() with coords = "ras" filters the box in RAS space', {
  vox2ras <- rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1));
  tracks <- list(
    cbind(c(70, 71), c(10, 10), c(0, 0)),   # RAS x ~ -8.5 .. -7.5
    cbind(c(120, 121), c(10, 10), c(0, 0))  # RAS x ~ -42.5 .. -41.5
  );
  fp <- tempfile(fileext = '.trk');
  write_test_trk(fp, tracks, vox2ras = vox2ras, voxel_order = 'LPS');

  # A box around the RAS position of the second track only.
  selected <- freesurferformats::read.dti.trk(fp, coords = 'ras', bbox = c(-50, -40, -100, 100, -100, 100));
  testthat::expect_equal(length(selected$tracks), 1L);
  testthat::expect_equal(freesurferformats::fs.tracts.coords(selected$tracks)[, 1], c(-41.5, -42.5), tolerance = 1e-3);
})


testthat::test_that('write.dti.tck() writes files that the reader and the spec agree on', {
  tracks <- make_test_tracks(5L, 3L);
  fp <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(tracks, fp);

  tck <- freesurferformats::read.dti.tck(fp);
  testthat::expect_equal(length(tck$tracks), 5L);
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(tck$tracks), rep(3L, 5L));
  for (track_idx in seq_along(tracks)) {
    testthat::expect_equal(tck$tracks[[track_idx]], tracks[[track_idx]], tolerance = 1e-4);
  }
  testthat::expect_equal(tck$header$count, 5L);

  # The payload must be byte-identical to the one written by the independent
  # implementation in the test helpers, which was validated against files
  # produced by MRtrix.
  fp_reference <- tempfile(fileext = '.tck');
  write_test_tck(fp_reference, tracks);
  offset_written <- freesurferformats::read.dti.tck.header(fp)$derived$data_offset;
  offset_reference <- freesurferformats::read.dti.tck.header(fp_reference)$derived$data_offset;
  written_bytes <- readBin(fp, 'raw', file.size(fp));
  reference_bytes <- readBin(fp_reference, 'raw', file.size(fp_reference));
  testthat::expect_equal(length(written_bytes) - offset_written, length(reference_bytes) - offset_reference);
  testthat::expect_equal(written_bytes[(offset_written + 1):length(written_bytes)],
                         reference_bytes[(offset_reference + 1):length(reference_bytes)]);
})


testthat::test_that('write.dti.tck() supports gzip and all datatypes', {
  tracks <- make_test_tracks(3L, 4L);

  for (datatype in c('Float32LE', 'Float32BE', 'Float64LE', 'Float64BE')) {
    fp <- tempfile(fileext = '.tck');
    freesurferformats::write.dti.tck(tracks, fp, datatype = datatype);
    tck <- freesurferformats::read.dti.tck(fp);
    tolerance <- if (startsWith(datatype, 'Float64')) 1e-12 else 1e-4;
    testthat::expect_equal(tck$tracks[[2]], tracks[[2]], tolerance = tolerance, info = datatype);
  }

  # gzip is selected from the file name, and can be forced.
  fp_gz <- tempfile(fileext = '.tck.gz');
  freesurferformats::write.dti.tck(tracks, fp_gz);
  testthat::expect_true(freesurferformats::read.dti.tck.header(fp_gz)$derived$gzipped);
  testthat::expect_equal(freesurferformats::read.dti.tck(fp_gz)$tracks[[1]], tracks[[1]], tolerance = 1e-4);

  fp_forced <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(tracks, fp_forced, gzip = TRUE);
  testthat::expect_true(freesurferformats::read.dti.tck.header(fp_forced)$derived$gzipped);

  testthat::expect_error(freesurferformats::write.dti.tck(tracks, tempfile(), datatype = 'Int16'), 'datatype');
})


testthat::test_that('write.dti.tck() stores and preserves the header entries', {
  fp <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(make_test_tracks(2L, 2L), fp,
                                   header = list(method = 'iFOD2', step_size = '1.25'));

  header <- freesurferformats::read.dti.tck.header(fp);
  testthat::expect_equal(header$method, 'iFOD2');
  testthat::expect_equal(header$step_size, 1.25);

  # Entries that the writer has to compute are not taken from the input header.
  fp2 <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(make_test_tracks(2L, 2L), fp2,
                                   header = list(count = '999', datatype = 'Nonsense'));
  header2 <- freesurferformats::read.dti.tck.header(fp2);
  testthat::expect_equal(header2$count, 2L);
  testthat::expect_equal(header2$datatype, 'Float32LE');
})


testthat::test_that('write.dti.trk() writes files that the reader can read back', {
  tracks <- make_test_tracks(4L, 3L);
  header <- list(vox2ras = rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1)),
                 voxel_order = 'LPS', voxel_size = c(1, 1, 1), dim = c(10L, 10L, 10L));

  fp <- tempfile(fileext = '.trk');
  freesurferformats::write.dti.trk(tracks, fp, header = header);

  trk <- freesurferformats::read.dti.trk(fp, coords = 'native');
  testthat::expect_equal(length(trk$tracks), 4L);
  testthat::expect_equal(trk$header$n_count, 4L);
  testthat::expect_equal(trk$header$voxel_order, 'LPS');
  testthat::expect_equal(trk$header$dim, c(10L, 10L, 10L));
  testthat::expect_equal(trk$header$vox2ras, header$vox2ras, tolerance = 1e-6);
  for (track_idx in seq_along(tracks)) {
    testthat::expect_equal(trk$tracks[[track_idx]]$coords, tracks[[track_idx]], tolerance = 1e-4);
  }

  # The file consists of the header and one record per track.
  expected_size <- 1000 + sum(vapply(tracks, function(t) 4L + nrow(t) * 3L * 4L, integer(1L)));
  testthat::expect_equal(file.size(fp), expected_size);

  # Big endian files round trip as well.
  fp_be <- tempfile(fileext = '.trk');
  freesurferformats::write.dti.trk(tracks, fp_be, header = header, endian = 'big');
  trk_be <- freesurferformats::read.dti.trk(fp_be, coords = 'native');
  testthat::expect_equal(trk_be$tracks[[1]]$coords, tracks[[1]], tolerance = 1e-4);
  testthat::expect_error(freesurferformats::write.dti.trk(tracks, tempfile(), endian = 'middle'), 'endian');
})


testthat::test_that('write.dti.trk() stores scalars and properties', {
  tract_data <- freesurferformats::fs.tracts(
    matrix(c(1, 2, 3, 4, 5, 6), ncol = 3, byrow = TRUE),
    c(1L, 1L),
    scalars = matrix(c(10, 20, 11, 21), ncol = 2),
    properties = matrix(c(100, 200), ncol = 1),
    kind = 'trk'
  );
  fp <- tempfile(fileext = '.trk');
  freesurferformats::write.dti.trk(tract_data, fp);
  trk <- freesurferformats::read.dti.trk(fp);

  testthat::expect_equal(trk$header$n_scalars, 2L);
  testthat::expect_equal(trk$header$n_properties, 1L);
  testthat::expect_equal(as.vector(trk$tracks[[2]]$scalars), c(20, 21), tolerance = 1e-4);
  testthat::expect_equal(trk$tracks[[1]]$properties, 100, tolerance = 1e-4);
  testthat::expect_equal(trk$tracks[[2]]$properties, 200, tolerance = 1e-4);
  testthat::expect_equal(trk$tracks[[1]]$coords, matrix(c(1, 2, 3), nrow = 1L), tolerance = 1e-4);
})


testthat::test_that('a read-modify-write round trip through RAS works', {
  vox2ras <- rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1));
  header <- list(vox2ras = vox2ras, voxel_order = 'LPS', voxel_size = c(1, 1, 1), dim = c(10L, 10L, 10L));
  tracks <- make_test_tracks(3L, 4L);

  fp <- tempfile(fileext = '.trk');
  freesurferformats::write.dti.trk(tracks, fp, header = header);

  # Read in RAS space, write back without further arguments.
  trk_ras <- freesurferformats::read.dti.trk(fp, coords = 'ras');
  fp2 <- tempfile(fileext = '.trk');
  freesurferformats::write.dti.trk(trk_ras$tracks, fp2, header = trk_ras$header);

  # The written file must contain the same coordinates as the original one.
  trk_again <- freesurferformats::read.dti.trk(fp2, coords = 'ras');
  testthat::expect_equal(freesurferformats::fs.tracts.coords(trk_again$tracks),
                         freesurferformats::fs.tracts.coords(trk_ras$tracks), tolerance = 1e-3);
  testthat::expect_equal(trk_again$header$vox2ras, vox2ras, tolerance = 1e-6);
})


testthat::test_that('reading a subset and writing it out is a working crop workflow', {
  tracks <- make_test_tracks(20L, 5L);
  fp <- tempfile(fileext = '.tck');
  write_test_tck(fp, tracks);

  # Take 4 tracks starting at the 6th, and write them to a new file.
  subset_tck <- freesurferformats::read.dti.tck(fp, skip_tracks = 5L, max_tracks = 4L);
  fp_subset <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(subset_tck$tracks, fp_subset, header = subset_tck$header);

  reread <- freesurferformats::read.dti.tck(fp_subset);
  testthat::expect_equal(length(reread$tracks), 4L);
  for (track_idx in seq_along(reread$tracks)) {
    testthat::expect_equal(reread$tracks[[track_idx]], tracks[[5L + track_idx]], tolerance = 1e-4);
  }
})


testthat::test_that('an empty tractogram can be written and read back', {
  empty_tracts <- freesurferformats::fs.tracts(matrix(numeric(0L), nrow = 0L, ncol = 3L), integer(0L), kind = 'tck');
  fp <- tempfile(fileext = '.tck');
  freesurferformats::write.dti.tck(empty_tracts, fp);
  reread <- freesurferformats::read.dti.tck(fp);
  testthat::expect_equal(length(reread$tracks), 0L);
  testthat::expect_equal(nrow(freesurferformats::fs.tracts.coords(reread$tracks)), 0L);
  testthat::expect_equal(freesurferformats::dti.track.count(fp), 0L);
})


testthat::test_that('as.fs.tracts() accepts the documented inputs', {
  tracks <- make_test_tracks(3L, 2L);

  from_list <- freesurferformats::as.fs.tracts(tracks);
  testthat::expect_true(freesurferformats::is.fs.tracts(from_list));
  testthat::expect_equal(length(from_list), 3L);
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(from_list), rep(2L, 3L));

  from_matrix <- freesurferformats::as.fs.tracts(tracks[[1]]);
  testthat::expect_equal(length(from_matrix), 1L);
  testthat::expect_equal(from_matrix[[1]], tracks[[1]]);

  testthat::expect_equal(freesurferformats::as.fs.tracts(from_list), from_list);
  testthat::expect_error(freesurferformats::as.fs.tracts(list(1, 2, 3)), 'numeric matrix with 3 columns');
  testthat::expect_error(freesurferformats::as.fs.tracts(list()), 'fs.tracts instance');
})
