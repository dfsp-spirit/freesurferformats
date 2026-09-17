# Tests for the handling of very large tract files.
#
# The files that motivated the streaming TRK/TCK/TSF readers are whole-brain
# tractograms of several GB, e.g., the QSIRecon output used during development
# (10 million streamlines, 8.9 GB gzip-compressed, 9.7 GB uncompressed). They
# cannot be part of the test data: R packages have to stay below 5 MB to be
# accepted on CRAN, and even the optional 'extra_test_data' mechanism (see
# find_extra_test_data_file()) is only meant for tens of MB.
#
# The reader code never branches on the size of the file, though. It branches on
# the number of values that are held in memory, on the chunk boundaries, on the
# configured memory limit, and on the header text. All of these are driven here
# with files of a few hundred bytes:
#
#   * The chunk boundary tests read one small file with many different chunk
#     sizes. Whatever the chunk size, the result has to be identical, which is
#     exactly the property that a multi-GB file relies on. Sizes that are not
#     multiples of the 3 values per point, and sizes that split a separator or
#     the terminator across two chunks, are included on purpose.
#   * The memory limit is lowered instead of the file being enlarged, which makes
#     the very same code path trigger on a small file.
#   * A sparse file (one that reports several GB but occupies almost no disk
#     space) is used for the one place that does look at the file size, the
#     up-front check that refuses to load a file that cannot fit into memory.
#   * The header of the real 8.9 GB file is embedded below, so that parsing a
#     realistic header (34 entries, NUL padding before the payload) is covered.
#
# The remaining properties of the large files, i.e. throughput and a constant
# memory footprint over millions of streamlines, are checked against real files
# by the script dev_tools/check_large_tck.R.


# A real MRtrix header ---------------------------------------------------------
#
# Taken verbatim from the header of a QSIRecon/MRtrix3 whole-brain tractogram
# (sub-01_space-ACPC_model-ifod2_streamlines.tck, 10,000,000 streamlines,
# 1,868 bytes, of which 13 NUL bytes of padding before the payload). The local
# pipeline paths were replaced by '/data/recon' equivalents and the 'file' entry
# (which depends on the header length) is added by write_header_fixture_tck().
real_mrtrix_header_lines <- c(
  "mrtrix tracks",
  "act: /data/recon/sub-01_mrtrix_single_shell/recon_anatomical_wf_0/apply_header_to_5tt/sub-01_5tt_hdrxform.nii.gz",
  "backtrack: 1",
  "command_history: tckgen -act /data/recon/sub-01_mrtrix_single_shell/recon_anatomical_wf_0/apply_header_to_5tt/sub-01_5tt_hdrxform.nii.gz -algorithm iFOD2 -backtrack -crop_at_gmwmi -maxlength 250.000000 -minlength 30.000000 -samples 4 -nthreads 8 -output_seeds out_seeds.nii.gz -power 0.330000 -quiet -seed_dynamic /data/recon/sub-01_mrtrix_single_shell/sub_01_space_ACPC_desc_preproc_recon_wf/ss3t_csd/intensity_norm/sub-01_space_ACPC_desc-preproc_dwi_wm_mtnorm.mif -select 10000000 /data/recon/sub-01_mrtrix_single_shell/sub_01_space_ACPC_desc_preproc_recon_wf/ss3t_csd/intensity_norm/sub-01_space_ACPC_desc-preproc_dwi_wm_mtnorm.mif tracked.tck  (version=3.0.4)",
  "crop_at_gmwmi: 1",
  "downsample_factor: 3",
  "fod_power: 0.330000013",
  "init_threshold: 0.0500000007",
  "lmax: 8",
  "max_angle: 45",
  "max_dist: 250.000000",
  "max_num_seeds: 10000000000",
  "max_num_tracks: 10000000",
  "max_seed_attempts: 1000",
  "max_trials: 1000",
  "method: iFOD2",
  "min_dist: 30.000000",
  "mrtrix_version: 3.0.4",
  "rk4: 0",
  "samples_per_step: 4",
  "seed_dynamic: /data/recon/sub-01_mrtrix_single_shell/sub_01_space_ACPC_desc_preproc_recon_wf/ss3t_csd/intensity_norm/sub-01_space_ACPC_desc-preproc_dwi_wm_mtnorm.mif",
  "seed_output: out_seeds.nii.gz",
  "sh_precomputed: 1",
  "source: /data/recon/sub-01_mrtrix_single_shell/sub_01_space_ACPC_desc_preproc_recon_wf/ss3t_csd/intensity_norm/sub-01_space_ACPC_desc-preproc_dwi_wm_mtnorm.mif",
  "step_size: 1",
  "stop_on_all_include: 0",
  "threshold: 0.0500000007",
  "timestamp: 1788898338.171040535",
  "unidirectional: 0",
  "roi: seed sub-01_space_ACPC_desc-preproc_dwi_wm_mtnorm.mif",
  "datatype: Float32LE",
  "count: 10000000",
  "total_count: 22150817",
  "END"
)

# The keys of the header above, excluding 'file' (added by the writer) and 'id'
# (which the reader derives from the first line of the file).
real_mrtrix_header_keys <- unname(vapply(
  real_mrtrix_header_lines[grepl("^[a-z0-9_]+:", real_mrtrix_header_lines) &
                             !grepl("^file:", real_mrtrix_header_lines)],
  function(line) sub(":.*$", "", line), character(1), USE.NAMES = FALSE
))


# Helper functions -------------------------------------------------------------

# Writes a TCK file whose header is the character vector 'lines', followed by
# 'num_padding' NUL bytes and the payload of 'tracks'. This mimics what MRtrix
# does: it pads the header, so the payload does not start right after the 'END'
# line but at the byte offset declared in the 'file' entry. The entry itself is
# computed from the resulting header length, since its own number of digits
# contributes to that length.
#
# If 'apparent_bytes' is given, the file is extended with a hole up to that size
# (i.e., it becomes sparse on file systems that support this) and the EOF marker
# is written at its very end, so the file reports that size although almost no
# data is stored.
write_header_fixture_tck <- function(path, lines, tracks = list(), num_padding = 13L, gzip = FALSE,
                                     apparent_bytes = NULL, terminator = TRUE) {
  if (!is.null(apparent_bytes) && gzip) {
    stop("A gzip-compressed file cannot be sparse.")
  }
  lines <- lines[lines != "END"]

  payload <- numeric(0)
  for (track in tracks) {
    payload <- c(payload, as.numeric(t(track)), NaN, NaN, NaN)
  }

  offset <- 0L
  repeat {
    header <- paste0(paste(c(lines, sprintf("file: . %d", offset), "END"), collapse = "\n"), "\n")
    new_offset <- nchar(header, type = "bytes") + num_padding
    if (new_offset == offset) {
      break
    }
    offset <- new_offset
  }

  con <- if (gzip) gzfile(path, "wb") else file(path, "wb")
  on.exit({
    close(con)
  }, add = TRUE)
  writeBin(charToRaw(header), con)
  padding <- offset - nchar(header, type = "bytes")
  if (padding > 0L) {
    writeBin(as.raw(rep(0L, padding)), con)
  }
  writeBin(payload, con, size = 4L, endian = "little")
  if (!is.null(apparent_bytes)) {
    seek(con, where = apparent_bytes, origin = "start")
  }
  if (terminator || !is.null(apparent_bytes)) {
    writeBin(c(Inf, Inf, Inf), con, size = 4L, endian = "little")
  }
  return(invisible(offset))
}

# The disk space (in KB) that a file really occupies, as opposed to the size it
# reports. Used to detect whether a file is sparse. Returns NA if that cannot be
# determined.
disk_usage_kb <- function(path) {
  du <- suppressWarnings(system2("du", c("-k", shQuote(path)), stdout = TRUE, stderr = FALSE))
  if (length(du) != 1L) {
    return(NA_real_)
  }
  return(suppressWarnings(as.numeric(strsplit(trimws(du), "\\s+")[[1]][1])))
}

# Tracks with varying lengths, to make every chunk boundary fall somewhere else.
chunk_test_tracks <- function() {
  return(list(
    cbind(c(1, 2), c(1, 2), c(1, 2)),                 # 2 points
    cbind(seq(0.5, 5, length.out = 10), rep(0, 10), rep(0, 10)), # 10 points
    cbind(5, 5, 5),                                   # 1 point
    cbind(seq(1, 7), seq(1, 7), seq(1, 7)),           # 7 points
    cbind(c(-1, -2, -3), c(0, 0, 0), c(0, 0, 0))      # 3 points
  ))
}

expected_chunk_test_lengths <- function() {
  return(c(2L, 10L, 1L, 7L, 3L))
}

# Chunk sizes to test with. Some of them are not multiples of 3 (the number of
# values per point in TCK), which forces the reader to carry a value over into
# the next chunk, and they are small enough to split separators and the EOF
# marker across chunks.
chunk_test_sizes <- function() {
  return(c(1L, 2L, 3L, 4L, 5L, 7L, 11L, 13L, 23L, 69L, 1000L, 4e6))
}


# 1. Chunk boundaries ----------------------------------------------------------

testthat::test_that("the TCK reader returns the same result for any chunk size", {
  tracks <- chunk_test_tracks()
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, tracks)

  reference <- freesurferformats::read.dti.tck(fp)
  expected_coords <- freesurferformats::fs.tracts.coords(reference$tracks)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(reference$tracks), expected_chunk_test_lengths())
  testthat::expect_equal(nrow(expected_coords), 23L)

  for (chunk_values in chunk_test_sizes()) {
    info <- sprintf("chunk size %s", chunk_values)
    res <- freesurferformats::read.dti.tck(fp, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), expected_chunk_test_lengths(), info = info)
    testthat::expect_equal(freesurferformats::fs.tracts.coords(res$tracks), expected_coords, info = info)

    # The same for a reader that stops early, and for one that starts late: the
    # leftover values of a chunk must not affect which tract is which.
    res <- freesurferformats::read.dti.tck(fp, max_tracks = 3L, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(2L, 10L, 1L), info = info)

    res <- freesurferformats::read.dti.tck(fp, skip_tracks = 2L, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(1L, 7L, 3L), info = info)
  }
})

testthat::test_that("the TCK scanners and the iterator are independent of the chunk size", {
  tracks <- chunk_test_tracks()
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, tracks);

  all_coords <- do.call(rbind, tracks)
  expected_bbox <- c(min(all_coords[, 1]), max(all_coords[, 1]),
                     min(all_coords[, 2]), max(all_coords[, 2]),
                     min(all_coords[, 3]), max(all_coords[, 3]))

  for (chunk_values in chunk_test_sizes()) {
    info <- sprintf("chunk size %s", chunk_values)
    testthat::expect_equal(freesurferformats::dti.track.count(fp, chunk_values = chunk_values),
                           length(tracks), info = info)
    testthat::expect_equal(freesurferformats::dti.track.bbox(fp, chunk_values = chunk_values),
                           expected_bbox, tolerance = 1e-5, info = info)

    itr <- freesurferformats::dti.track.iterator(fp, chunk_values = chunk_values)
    lengths <- integer(0)
    while (!is.null(track <- itr$next.track())) {
      lengths <- c(lengths, nrow(track))
    }
    itr$close()
    testthat::expect_equal(lengths, expected_chunk_test_lengths(), info = info)
    testthat::expect_equal(itr$tracks.read, length(tracks), info = info)
  }
})

testthat::test_that("a tract that spans several chunks is read correctly", {
  # A single long tract, so that the chunked reader has to keep a group open
  # across many chunks.
  long_track <- cbind(seq(0, 100, length.out = 300), rep(7, 300), rep(0, 300))
  tracks <- list(long_track, cbind(c(1, 2), c(3, 4), c(5, 6)))
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, tracks)

  for (chunk_values in c(1L, 4L, 5L, 91L, 1000L)) {
    res <- freesurferformats::read.dti.tck(fp, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(300L, 2L),
                           info = sprintf("chunk size %s", chunk_values))
    testthat::expect_equal(freesurferformats::fs.tracts.coords(res$tracks)[1:300, ],
                           long_track, tolerance = 1e-5)
  }

  # A tract longer than one chunk must still be found by the bbox filter, whose
  # result is computed from the points of the open group.
  res <- freesurferformats::read.dti.tck(fp, bbox = c(50, 60, -1, 8, -1, 1), chunk_values = 10L)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), 300L)
})

testthat::test_that("the TSF reader is independent of the chunk size", {
  track_values <- list(seq(1, 2), seq(1, 10), 1, seq(1, 7), seq(1, 3))
  fp <- tempfile(fileext = ".tsf")
  write_test_tsf(fp, track_values)

  expected <- unlist(track_values)
  expected_lengths <- vapply(track_values, length, 0L)

  for (chunk_values in c(1L, 2L, 3L, 4L, 5L, 7L, 11L, 1000L, 4e6)) {
    info <- sprintf("chunk size %s", chunk_values)
    res <- freesurferformats::read.dti.tsf(fp, chunk_values = chunk_values)
    testthat::expect_equal(res$scalars$merged, expected, info = info)
    testthat::expect_equal(res$scalars$lengths, expected_lengths, info = info)
    testthat::expect_equal(unname(vapply(res$scalars$scalar_list, length, 0L)),
                           expected_lengths, info = info)

    # TSF files have one value per point, so a chunk size of 1 leaves a leftover
    # value after every chunk.
    itr <- freesurferformats::dti.track.iterator(fp, chunk_values = chunk_values)
    values <- numeric(0)
    while (!is.null(track <- itr$next.track())) {
      values <- c(values, track)
    }
    itr$close()
    testthat::expect_equal(values, expected, info = info)
    testthat::expect_equal(freesurferformats::dti.track.count(fp, chunk_values = chunk_values),
                           length(track_values), info = info)
  }
})

testthat::test_that("tract data split across a chunk boundary is not lost", {
  # A NaN separator and the Inf end marker are both split across a chunk
  # boundary here: with 3 values per point, a chunk size of 2 puts the first
  # separator at values 7 and 8, and the EOF marker's values end up in two
  # different chunks when the size is not a multiple of 3.
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)), cbind(c(3, 4), c(3, 4), c(3, 4)))
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, tracks, nan_after_last = TRUE, terminator = TRUE)

  for (chunk_values in c(2L, 4L, 5L, 8L, 14L)) {
    info <- sprintf("chunk size %s", chunk_values)
    res <- freesurferformats::read.dti.tck(fp, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(2L, 2L), info = info)
    testthat::expect_equal(as.vector(freesurferformats::fs.tracts.coords(res$tracks)[2, ]), c(2, 2, 2), info = info)
  }

  # The same file without the EOF marker, which must be accepted (see the note
  # in the reader) and must not depend on the chunk size either.
  fp_no_term <- tempfile(fileext = ".tck")
  write_test_tck(fp_no_term, tracks, terminator = FALSE)
  for (chunk_values in c(2L, 4L, 5L)) {
    res <- freesurferformats::read.dti.tck(fp_no_term, chunk_values = chunk_values)
    testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(2L, 2L),
                           info = sprintf("chunk size %s, no terminator", chunk_values))
  }
})

testthat::test_that("a header with a value that spans several lines is handled", {
  # MRtrix writes long command lines into a single header entry. Values may
  # contain spaces, and the reader must split at the first colon only.
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, list(cbind(c(1, 2), c(1, 2), c(1, 2))),
                 extra_header_lines = c("command_history: tckgen -act /data/5tt.nii.gz -algorithm iFOD2 (version=3.0.4)",
                                        "roi: seed C:\\data\\windows_path.mif"))

  header <- freesurferformats::read.dti.tck.header(fp)
  testthat::expect_equal(header$command_history,
                         "tckgen -act /data/5tt.nii.gz -algorithm iFOD2 (version=3.0.4)")
  # A colon inside the value (a Windows path here) must be kept.
  testthat::expect_equal(header$roi, "seed C:\\data\\windows_path.mif")
})


# 2. The memory limit ----------------------------------------------------------

# A TCK file with a payload of about 48 KB, used to check the allocation limit.
# 100 tracks of 20 points are 2000 points, i.e., 6000 values, and the readers
# count 8 bytes per value in memory, which is 48000 bytes.
write_limit_test_tck <- function(num_tracks = 100L, points_per_track = 20L) {
  set.seed(42)
  tracks <- replicate(num_tracks,
                      cbind(stats::runif(points_per_track, 0, 100),
                            stats::runif(points_per_track, 0, 100),
                            stats::runif(points_per_track, 0, 100)),
                      simplify = FALSE)
  fp <- tempfile(fileext = ".tck")
  write_test_tck(fp, tracks)
  # Each tract contributes its 3 values per point plus a NaN separator, and the
  # file ends with the EOF marker.
  num_values <- num_tracks * points_per_track * 3 + num_tracks * 3 + 3
  return(list(filepath = fp, tracks = tracks, num_values = num_values))
}

write_limit_test_trk <- function(num_tracks = 100L, points_per_track = 20L) {
  set.seed(42)
  tracks <- replicate(num_tracks,
                      cbind(stats::runif(points_per_track, 0, 100),
                            stats::runif(points_per_track, 0, 100),
                            stats::runif(points_per_track, 0, 100)),
                      simplify = FALSE)
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, tracks)
  return(list(filepath = fp, tracks = tracks))
}

testthat::test_that("the allocation limit is enforced for whole-file reads", {
  fixture <- write_limit_test_tck()

  old <- options(freesurferformats.max_alloc_bytes = 20000)
  on.exit(options(old), add = TRUE)

  # The message must name the size that was requested and the limit, and it must
  # not report small sizes as '0.00 GB'.
  expected_requested <- sprintf("%.2f KB", fixture$num_values * 8 / 1e3)
  testthat::expect_error(freesurferformats::read.dti.tck(fixture$filepath),
                         paste0(expected_requested, ".*20\\.00 KB"))
  testthat::expect_error(freesurferformats::read.dti.tck(fixture$filepath),
                         "exceeds the safety limit")

  # Raising the limit makes the very same file readable.
  options(freesurferformats.max_alloc_bytes = 1e6)
  res <- freesurferformats::read.dti.tck(fixture$filepath)
  testthat::expect_equal(length(res$tracks), 100L)
  testthat::expect_equal(nrow(freesurferformats::fs.tracts.coords(res$tracks)), 2000L)
})

testthat::test_that("the memory limit does not stop the streaming entry points", {
  # This is a regression test for a real bug: the up-front check for whole-file
  # reads used to be applied by the readers of subsets and by the streaming
  # helpers as well, so a large file could not be read at all, not even the
  # first track. A tiny limit must only affect whole-file reads.
  fixture <- write_limit_test_tck()
  old <- options(freesurferformats.max_alloc_bytes = 20000)
  on.exit(options(old), add = TRUE)

  subset <- freesurferformats::read.dti.tck(fixture$filepath, max_tracks = 5L, chunk_values = 100L)
  testthat::expect_equal(length(subset$tracks), 5L)
  testthat::expect_equal(freesurferformats::fs.tracts.coords(subset$tracks),
                         do.call(rbind, fixture$tracks[1:5]), tolerance = 1e-4)

  testthat::expect_equal(freesurferformats::dti.track.count(fixture$filepath, chunk_values = 100L), 100L)
  bbox <- freesurferformats::dti.track.bbox(fixture$filepath, chunk_values = 100L)
  all_coords <- do.call(rbind, fixture$tracks)
  testthat::expect_equal(bbox[1], min(all_coords[, 1]), tolerance = 1e-4)

  itr <- freesurferformats::dti.track.iterator(fixture$filepath, chunk_values = 100L)
  testthat::expect_equal(nrow(itr$next.track()), 20L)
  itr$close()

  # The same for a TRK file, whose records are read one at a time.
  fixture_trk <- write_limit_test_trk()
  trk <- freesurferformats::read.dti.trk(fixture_trk$filepath, max_tracks = 5L)
  testthat::expect_equal(length(trk$tracks), 5L)
  testthat::expect_equal(freesurferformats::dti.track.count(fixture_trk$filepath), 100L)
  itr_trk <- freesurferformats::dti.track.iterator(fixture_trk$filepath)
  testthat::expect_equal(itr_trk$next.track()$num_points, 20L)
  itr_trk$close()
})

testthat::test_that("a TRK record with an absurd point count is rejected", {
  # A single corrupt record can claim a point count that would need tens of GB.
  # The reader must refuse it instead of trying to allocate, and it must do so
  # before reading anything. No big file is needed for this test.
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, list(), n_count = 1L)
  con <- file(fp, "ab")
  writeBin(as.integer(1000000000L), con, size = 4L, endian = "little")
  close(con)

  testthat::expect_error(freesurferformats::read.dti.trk(fp),
                         "a single TRK track record")
  testthat::expect_error(freesurferformats::read.dti.trk(fp),
                         "exceeds the safety limit")
})

testthat::test_that("a file larger than the memory limit is refused without being read", {
  testthat::skip_on_os("windows") # sparse files are not portable, and a real 4 GB write would be bad

  # A sparse file reports 4 GB, but the bytes in the middle are a hole that
  # costs no disk space and reads back as zeros. This is the only way to test
  # the size-dependent behaviour without shipping a huge file. Probe first: on a
  # file system without sparse file support the tests are skipped, rather than
  # filling up the disk.
  probe <- tempfile()
  con <- file(probe, "wb")
  writeBin(as.raw(1L), con)
  seek(con, where = 100e6, origin = "start")
  writeBin(as.raw(1L), con)
  close(con)
  probe_kb <- disk_usage_kb(probe)
  unlink(probe)
  testthat::skip_if(is.na(probe_kb) || probe_kb > 1e5,
                    message = "the file system does not support sparse files, skipping")

  fp <- tempfile(fileext = ".tck")
  on.exit(unlink(fp), add = TRUE)

  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)), cbind(c(3, 4), c(3, 4), c(3, 4)))
  apparent_bytes <- 4e9
  offset <- write_header_fixture_tck(fp, real_mrtrix_header_lines, tracks = tracks,
                                     apparent_bytes = apparent_bytes)
  testthat::expect_true(file.size(fp) > apparent_bytes)
  testthat::expect_true(disk_usage_kb(fp) < 1e5) # really is sparse

  # Reading the header is instant, since only the first bytes are read.
  header <- freesurferformats::read.dti.tck.header(fp)
  testthat::expect_equal(header$datatype, "Float32LE")
  testthat::expect_equal(header$derived$data_offset, offset)

  # Requesting all of it is refused with the safety message, since the payload
  # cannot fit into memory (it would need 8 GB, the limit is 2 GB).
  testthat::expect_error(freesurferformats::read.dti.tck(fp), "exceeds the safety limit")
  testthat::expect_error(freesurferformats::read.dti.tck(fp), "8\\.00 GB")

  # A subset of the first tracts is read without ever touching the 4 GB hole, so
  # this returns immediately even though the file is bigger than the RAM.
  subset <- freesurferformats::read.dti.tck(fp, max_tracks = 2L, chunk_values = 100L)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(subset$tracks), c(2L, 2L))
  testthat::expect_equal(freesurferformats::fs.tracts.coords(subset$tracks)[1, ], c(1, 1, 1), tolerance = 1e-5)
})


# 3. Real world headers --------------------------------------------------------

testthat::test_that("a real-world MRtrix header is parsed", {
  fp <- tempfile(fileext = ".tck")
  offset <- write_header_fixture_tck(fp, real_mrtrix_header_lines)

  # The payload does not start right after 'END': there is padding in between,
  # and the reader has to use the byte offset declared in the 'file' entry.
  text_without_file_entry <- paste0(paste(real_mrtrix_header_lines[real_mrtrix_header_lines != "END"],
                                          collapse = "\n"), "\n")
  testthat::expect_true(offset > nchar(text_without_file_entry, type = "bytes"))

  header <- freesurferformats::read.dti.tck.header(fp)
  testthat::expect_equal(sort(names(header)), sort(c(real_mrtrix_header_keys, "id", "file", "derived")))
  testthat::expect_equal(length(real_mrtrix_header_keys), 32L)
  testthat::expect_equal(header$id, "mrtrix tracks")
  testthat::expect_equal(header$datatype, "Float32LE")
  testthat::expect_equal(header$count, 10000000)
  testthat::expect_equal(header$total_count, 22150817)
  testthat::expect_equal(header$max_num_tracks, 10000000)
  testthat::expect_equal(header$method, "iFOD2")
  testthat::expect_equal(header$mrtrix_version, "3.0.4")
  testthat::expect_equal(header$derived$data_offset, offset)
  testthat::expect_false(header$derived$gzipped)
  testthat::expect_equal(header$derived$dsize, 4L)
  # The long values must be kept completely, with their inner spaces and their
  # embedded path separators.
  testthat::expect_true(grepl("-algorithm iFOD2", header$command_history, fixed = TRUE))
  testthat::expect_true(grepl("/data/recon", header$command_history, fixed = TRUE))
  testthat::expect_true(grepl("out_seeds.nii.gz", header$seed_output, fixed = TRUE))
})

testthat::test_that("the header count is compared against the data actually found", {
  # A truncated file: the header of the real 10 million tractogram with only two
  # tracts in it, and without the end-of-file marker (the copy was cut off).
  # Reading it must warn about the mismatch, but must return the two tracts.
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)), cbind(c(3, 4), c(3, 4), c(3, 4)))
  fp <- tempfile(fileext = ".tck")
  write_header_fixture_tck(fp, real_mrtrix_header_lines, tracks = tracks, terminator = FALSE)

  testthat::expect_warning(res <- freesurferformats::read.dti.tck(fp), "10000000")
  testthat::expect_warning(res <- freesurferformats::read.dti.tck(fp), "may be truncated")
  testthat::expect_equal(length(res$tracks), 2L)

  # Requesting a subset must not warn, since then the reader is asked to stop
  # early on purpose.
  n_warnings <- 0L
  withCallingHandlers(
    freesurferformats::read.dti.tck(fp, max_tracks = 1L),
    warning = function(w) {
      n_warnings <<- n_warnings + 1L
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_equal(n_warnings, 0L)
})

testthat::test_that("a real-world header is parsed from a gzip-compressed file", {
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)))
  fp <- tempfile(fileext = ".tck.gz")
  offset <- write_header_fixture_tck(fp, real_mrtrix_header_lines, tracks = tracks, gzip = TRUE)

  header <- freesurferformats::read.dti.tck.header(fp)
  testthat::expect_true(header$derived$gzipped)
  testthat::expect_equal(header$derived$data_offset, offset)
  testthat::expect_equal(header$mrtrix_version, "3.0.4")

  # For a compressed file the reader cannot seek to the payload, it has to skip
  # the header bytes by reading them. The header is 1.8 KB here, so the
  # padding bytes are skipped in a single read.
  res <- suppressWarnings(freesurferformats::read.dti.tck(fp))
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), 2L)
  testthat::expect_equal(freesurferformats::fs.tracts.coords(res$tracks)[2, ], c(2, 2, 2), tolerance = 1e-5)
})


# 4. Empty tracts --------------------------------------------------------------

testthat::test_that("empty TRK tracts are read, counted and iterated", {
  # The TRK format allows a tract without any point, and 'nibabel' returns such
  # tracts. Their properties still have to be consumed, otherwise the reader
  # would lose the sync with the file and read garbage afterwards.
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)),
                 matrix(numeric(0), ncol = 3),
                 cbind(c(5, 6), c(5, 6), c(5, 6)))
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, tracks, n_properties = 2L)

  trk <- freesurferformats::read.dti.trk(fp)
  testthat::expect_equal(length(trk$tracks), 3L)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(trk$tracks), c(2L, 0L, 2L))
  testthat::expect_equal(dim(trk$tracks[[2]]$coords), c(0L, 3L))
  testthat::expect_equal(trk$tracks[[2]]$num_points, 0L)
  # The properties of the empty tract, and those of the following tract, are
  # only correct if the record boundaries were handled correctly.
  testthat::expect_equal(trk$tracks[[2]]$properties, c(20001, 20002), tolerance = 1e-4)
  testthat::expect_equal(trk$tracks[[3]]$properties, c(30001, 30002), tolerance = 1e-4)
  testthat::expect_equal(trk$tracks[[3]]$coords[1, ], c(5, 5, 5), tolerance = 1e-4)

  # The scanner and the iterator must agree with the reader.
  testthat::expect_equal(freesurferformats::dti.track.count(fp), 3L)
  bbox <- freesurferformats::dti.track.bbox(fp)
  testthat::expect_equal(bbox, c(1, 6, 1, 6, 1, 6), tolerance = 1e-5)

  itr <- freesurferformats::dti.track.iterator(fp)
  points <- integer(0)
  while (!is.null(track <- itr$next.track())) {
    points <- c(points, track$num_points)
  }
  itr$close()
  testthat::expect_equal(points, c(2L, 0L, 2L))

  # Skipping counts the empty tract as well.
  skipped <- freesurferformats::read.dti.trk(fp, skip_tracks = 1L)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(skipped$tracks), c(0L, 2L))

  # A bbox filter drops it, since none of its (zero) points can be inside.
  filtered <- freesurferformats::read.dti.trk(fp, bbox = c(-100, 100, -100, 100, -100, 100))
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(filtered$tracks), c(2L, 2L))

  # Reading a specific empty tract from the container works.
  testthat::expect_equal(nrow(trk$tracks[[2]]$coords), 0L)
  testthat::expect_equal(nrow(as.list(trk$tracks)[[2]]$coords), 0L)
})

testthat::test_that("empty TRK tracts survive a write-read round trip", {
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)),
                 matrix(numeric(0), ncol = 3),
                 cbind(c(5, 6), c(5, 6), c(5, 6)))
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, tracks, n_properties = 2L)
  trk <- freesurferformats::read.dti.trk(fp)

  out <- tempfile(fileext = ".trk")
  freesurferformats::write.dti.trk(trk$tracks, out, header = trk$header)
  back <- freesurferformats::read.dti.trk(out)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(back$tracks), c(2L, 0L, 2L))
  testthat::expect_equal(back$tracks[[2]]$properties, c(20001, 20002), tolerance = 1e-4)
  testthat::expect_equal(freesurferformats::fs.tracts.coords(back$tracks),
                         freesurferformats::fs.tracts.coords(trk$tracks))
})

testthat::test_that("write.dti.tck() warns about empty tracts", {
  # The TCK format cannot store a tract without points: it writes a bare
  # separator, which every reader drops again ('nibabel' does the same), so the
  # file would read back with fewer tracts than it was written from.
  tracks <- list(cbind(c(1, 2), c(1, 2), c(1, 2)),
                 matrix(numeric(0), ncol = 3),
                 cbind(c(5, 6), c(5, 6), c(5, 6)))
  fp <- tempfile(fileext = ".tck")
  testthat::expect_warning(freesurferformats::write.dti.tck(tracks, fp),
                           "1 of the 3 tracts to write are empty")
  testthat::expect_equal(length(freesurferformats::read.dti.tck(fp)$tracks), 2L)

  # Without empty tracts there is no warning.
  n_warnings <- 0L
  withCallingHandlers(
    freesurferformats::write.dti.tck(tracks[c(1, 3)], tempfile(fileext = ".tck")),
    warning = function(w) {
      n_warnings <<- n_warnings + 1L
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_equal(n_warnings, 0L)
})

testthat::test_that("the TCK readers and writers handle a file without any tract", {
  fp <- tempfile(fileext = ".tck")
  # An empty tractogram, built with the documented constructor. Note that an
  # empty list is not accepted by as.fs.tracts(), see its tests.
  empty <- freesurferformats::fs.tracts(matrix(numeric(0), ncol = 3), integer(0))
  testthat::expect_equal(length(empty), 0L)
  testthat::expect_silent(freesurferformats::write.dti.tck(empty, fp))
  header <- freesurferformats::read.dti.tck.header(fp)
  testthat::expect_equal(header$count, 0)
  testthat::expect_equal(header$derived$data_offset, file.size(fp) - 12L) # payload is only the EOF marker
  res <- freesurferformats::read.dti.tck(fp)
  testthat::expect_equal(length(res$tracks), 0L)
  testthat::expect_equal(freesurferformats::dti.track.count(fp), 0L)
  testthat::expect_null(freesurferformats::dti.track.bbox(fp))
  itr <- freesurferformats::dti.track.iterator(fp)
  testthat::expect_null(itr$next.track())
  itr$close()
})


# 5. Corrupt and truncated files -----------------------------------------------

test_that("a TRK record that is cut off in the middle is reported", {
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, list(cbind(seq(1, 5), seq(1, 5), seq(1, 5))))
  raw <- readBin(fp, what = "raw", n = file.size(fp))
  # Drop the last two of the five points, so the record promises more than it has.
  writeBin(raw[seq_len(length(raw) - 2L * 3L * 4L)], fp)

  testthat::expect_error(freesurferformats::read.dti.trk(fp), "Truncated TRK file")
  testthat::expect_error(freesurferformats::dti.track.bbox(fp), "Truncated TRK file")
  itr <- freesurferformats::dti.track.iterator(fp)
  testthat::expect_error(itr$next.track(), "Truncated TRK file")
  itr$close()
})

test_that("a TRK file that ends before the count stated in the header is reported", {
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, list(cbind(c(1, 2), c(1, 2), c(1, 2))), n_count = 3L)

  testthat::expect_error(freesurferformats::read.dti.trk(fp), "the header states 3 tracks")

  # Reading only one tract is what the user asked for, so this must not complain.
  res <- freesurferformats::read.dti.trk(fp, max_tracks = 1L)
  testthat::expect_equal(length(res$tracks), 1L)
  testthat::expect_equal(freesurferformats::dti.track.count(fp), 1L)
})

test_that("an invalid TRK point count is reported by all entry points", {
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, list(), n_count = 1L)
  con <- file(fp, "ab")
  writeBin(as.integer(-5L), con, size = 4L, endian = "little")
  close(con)

  testthat::expect_error(freesurferformats::read.dti.trk(fp), "Invalid point count")
  testthat::expect_error(freesurferformats::dti.track.count(fp), "Invalid point count")
  testthat::expect_error(freesurferformats::dti.track.bbox(fp), "Invalid point count")
  itr <- freesurferformats::dti.track.iterator(fp)
  testthat::expect_error(itr$next.track(), "Invalid point count")
  itr$close()
})

test_that("a file that is not a tract file at all is reported", {
  fp <- tempfile(fileext = ".tck")
  writeLines("this is not a tract file", fp)
  testthat::expect_error(freesurferformats::dti.track.count(fp), "not in TRK, TCK or TSF format")
  testthat::expect_error(freesurferformats::dti.track.iterator(fp), "not in TRK, TCK or TSF format")
  testthat::expect_error(freesurferformats::dti.track.count(tempfile()), "does not exist")
})

test_that("the iterator rejects a bbox for TSF files", {
  fp <- tempfile(fileext = ".tsf")
  write_test_tsf(fp, list(seq(1, 3), seq(1, 2)))
  testthat::expect_error(freesurferformats::dti.track.iterator(fp, bbox = c(0, 1, 0, 1, 0, 1)),
                         "cannot be used with them")
  # Skipping works for TSF files, though.
  itr <- freesurferformats::dti.track.iterator(fp, skip_tracks = 1L)
  testthat::expect_equal(itr$next.track(), c(1, 2))
  itr$close()
})

test_that("an empty tract preserves the tract count of a TRK file", {
  # Regression test: empty tracts used to be skipped without consuming their
  # properties, which made the reader lose the sync with the file, and they were
  # not counted, so a file with empty tracts looked truncated.
  tracks <- list(matrix(numeric(0), ncol = 3), matrix(numeric(0), ncol = 3))
  fp <- tempfile(fileext = ".trk")
  write_test_trk(fp, tracks, n_count = 2L, n_properties = 1L)

  res <- freesurferformats::read.dti.trk(fp) # must not report a truncated file
  testthat::expect_equal(length(res$tracks), 2L)
  testthat::expect_equal(freesurferformats::fs.tracts.lengths(res$tracks), c(0L, 0L))
  testthat::expect_equal(freesurferformats::dti.track.count(fp), 2L)
  testthat::expect_null(freesurferformats::dti.track.bbox(fp))
  testthat::expect_equal(res$tracks[[2]]$properties, 20001, tolerance = 1e-4)
})


# 6. Internal helpers ----------------------------------------------------------

test_that("groups.in.bbox() returns one entry per tract, including empty ones", {
  points <- rbind(c(0, 0, 0), c(10, 10, 10))
  box <- c(-1, 1, -1, 1, -1, 1)
  # Tract 1 is empty, tract 2 has the first point (inside), tract 3 is empty and
  # tract 4 has the second point (outside).
  res <- freesurferformats:::groups.in.bbox(points, c(0L, 1L, 0L, 1L), box)
  testthat::expect_equal(length(res), 4L)
  testthat::expect_equal(res, c(FALSE, TRUE, FALSE, FALSE))

  testthat::expect_equal(freesurferformats:::groups.in.bbox(points, c(0L, 0L), box), c(FALSE, FALSE))
  testthat::expect_equal(freesurferformats:::groups.in.bbox(points, integer(0), box), logical(0))
})

test_that("format_bytes_human() names small sizes properly", {
  testthat::expect_equal(freesurferformats:::format_bytes_human(0), "0 bytes")
  testthat::expect_equal(freesurferformats:::format_bytes_human(999), "999 bytes")
  testthat::expect_equal(freesurferformats:::format_bytes_human(1000), "1.00 KB")
  testthat::expect_equal(freesurferformats:::format_bytes_human(20000), "20.00 KB")
  testthat::expect_equal(freesurferformats:::format_bytes_human(2e7), "20.00 MB")
  testthat::expect_equal(freesurferformats:::format_bytes_human(2e9), "2.00 GB")
  testthat::expect_equal(freesurferformats:::format_bytes_human(8.91e9), "8.91 GB")
  testthat::expect_equal(freesurferformats:::format_bytes_human(Inf), "unlimited")
})
