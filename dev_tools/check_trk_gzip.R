# Cross-validate the gzip support for TrackVis TRK files against nibabel.
#
# TrackVis and MRtrix do not read compressed track files, so the compression is a storage feature: it is useful
# for archiving huge tractograms and for passing them between the programs of this project, and it is why the
# compression is detected from the file content rather than from the file name here. The reference
# implementation used below is nibabel, which reads gzip-compressed TRK files. (It cannot write them, so a
# compressed file is always produced by compressing an uncompressed one.)
#
# The checks are:
#   1. A compressed file and its uncompressed original must be read as the same tractogram. This uses the
#      real-world test data of the repository, an XTRACT tractogram, when it is available.
#   2. Files written by this package, compressed and uncompressed, must be read as the same tractogram by
#      nibabel, which shares no code with this package.
#   3. Every code path that reads a TRK file has to handle compression, since R cannot seek in a compressed
#      connection while the readers skip over data that they do not need: the track reader, the header reader,
#      the track counter, the bounding box scanner and the streaming iterator.
#
# The Python part is optional: the script reports when it cannot find nibabel and skips those checks. Run it
# with
#   Rscript dev_tools/check_trk_gzip.R
# and use the environment variable TRK_PYTHON to point to a non-standard Python installation.

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
repo_root <- if (length(script_file) == 1L) normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- getwd()
}
devtools::load_all(repo_root, quiet = TRUE)

find_python_with <- function(imports) {
  candidates <- c(
    Sys.getenv("TRK_PYTHON", unset = NA),
    path.expand("~/develop/brain_atlases/.venv/bin/python"),
    Sys.which("python3"),
    Sys.which("python")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    ok <- suppressWarnings(system2(candidate, c("-c", shQuote(sprintf("import %s", imports))), stdout = FALSE, stderr = FALSE))
    if (identical(as.integer(ok), 0L)) {
      return(candidate)
    }
  }
  return(NULL)
}

run_python <- function(python, code) {
  script_path <- tempfile(fileext = ".py")
  writeLines(code, script_path)
  on.exit(unlink(script_path), add = TRUE)
  output <- suppressWarnings(system2(python, script_path, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  return(list(status = status, output = paste(output, collapse = "\n")))
}

# Let nibabel dump the streamlines of a TRK file as one line per point, and read such a dump back into a
# coordinate matrix in the track order of the file.
nibabel_dump_streamlines <- function(python, trk_file, dump_file) {
  python_code <- sprintf(
    paste(
      "import nibabel as nib",
      "f = nib.streamlines.load(%s)",
      "with open(%s, 'w') as fh:",
      "    for streamline in f.streamlines:",
      "        for point in streamline:",
      "            fh.write('point %%.9g %%.9g %%.9g\\n' %% (point[0], point[1], point[2]))",
      sep = "\n"
    ),
    shQuote(trk_file), shQuote(dump_file)
  )
  return(run_python(python, python_code))
}

read_dump_coords <- function(dump_file) {
  tokens <- strsplit(readLines(dump_file), " ")
  values <- as.numeric(unlist(lapply(tokens, function(tokens) tokens[2:4])))
  return(t(matrix(values, nrow = 3L)))
}

tally <- function(status, checks, failures) {
  if (status == "OK") {
    checks <- checks + 1L
  } else if (status == "MISMATCH") {
    checks <- checks + 1L
    failures <- failures + 1L
  }
  return(list("checks" = checks, "failures" = failures))
}

report <- function(check_name, tool_label, status, detail = "") {
  cat(sprintf("   [%-26s] %-28s : %-8s %s\n", check_name, tool_label, status, detail))
  return(status)
}

find_extra_test_data_file <- function(relpath) {
  candidates <- c(
    file.path(repo_root, "extra_test_data", relpath),
    file.path(getwd(), "extra_test_data", relpath)
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  return(NULL)
}

checks <- 0L
failures <- 0L
skipped <- 0L

nibabel_python <- find_python_with("nibabel")

work_dir <- tempfile("trk_gzip_check_")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

real_trk <- find_extra_test_data_file(file.path("tracts", "STR_R.trk"))
real_trk_gz <- find_extra_test_data_file(file.path("tracts", "STR_R.trk.gz"))

# The tractogram used for the checks that do not need the test data.
if (is.null(real_trk)) {
  set.seed(42L)
  generated_tracks <- replicate(6L, cbind(stats::runif(5L, 0, 100), stats::runif(5L, 0, 100), stats::runif(5L, 0, 100)),
                                simplify = FALSE)
  reference_tracks <- as.fs.tracts(generated_tracks)
  reference_header <- list(vox2ras = rbind(c(-1, 0, 0, 78), c(0, -1, 0, 76), c(0, 0, 1, -50), c(0, 0, 0, 1)),
                           voxel_order = "LPS", voxel_size = c(1, 1, 1), dim = c(10L, 10L, 10L))
} else {
  loaded <- read.dti.trk(real_trk, coords = "native")
  reference_tracks <- loaded$tracks
  reference_header <- loaded$header
}

# Part 1: a compressed file and its original are the same tractogram -------------
cat("=== 1. A compressed file and its uncompressed original ===\n")
if (is.null(real_trk) || is.null(real_trk_gz)) {
  cat("   SKIPPED: the test data 'extra_test_data/tracts/STR_R.trk[.gz]' was not found.\n")
  skipped <- skipped + 1L
} else {
  plain <- read.dti.trk(real_trk, coords = "native")
  compressed <- read.dti.trk(real_trk_gz, coords = "native")

  problems <- character(0L)
  if (!identical(fs.tracts.coords(compressed$tracks), fs.tracts.coords(plain$tracks))) {
    problems <- c(problems, "the coordinates differ")
  }
  if (!identical(fs.tracts.lengths(compressed$tracks), fs.tracts.lengths(plain$tracks))) {
    problems <- c(problems, "the track lengths differ")
  }
  if (!identical(compressed$header, plain$header)) {
    problems <- c(problems, "the headers differ")
  }
  if (!identical(dti.track.count(real_trk_gz), dti.track.count(real_trk))) {
    problems <- c(problems, "the track counts differ")
  }
  if (!identical(dti.track.bbox(real_trk_gz), dti.track.bbox(real_trk))) {
    problems <- c(problems, "the bounding boxes differ")
  }
  plain_subset <- read.dti.trk(real_trk, skip_tracks = 5L, coords = "native")
  compressed_subset <- read.dti.trk(real_trk_gz, skip_tracks = 5L, coords = "native")
  if (!identical(fs.tracts.coords(compressed_subset$tracks), fs.tracts.coords(plain_subset$tracks))) {
    problems <- c(problems, "skipping tracks gives a different result")
  }

  status <- report("XTRACT tractogram", "this package", if (length(problems) == 0L) "OK" else "MISMATCH",
                   if (length(problems) == 0L) {
                     sprintf("%d tracks, %d points, identical", length(plain$tracks), nrow(fs.tracts.coords(plain$tracks)))
                   } else {
                     paste(problems, collapse = "; ")
                   })
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures
}

# Part 2: nibabel reads the compressed files of this package --------------------
cat("=== 2. Files written by this package, read by nibabel ===\n")
if (is.null(nibabel_python)) {
  cat("   SKIPPED: no Python interpreter with nibabel was found.\n")
  skipped <- skipped + 1L
} else {
  plain_file <- file.path(work_dir, "written.trk")
  gz_file <- file.path(work_dir, "written.trk.gz")
  write.dti.trk(reference_tracks, plain_file, header = reference_header)
  write.dti.trk(reference_tracks, gz_file, header = reference_header)

  expected_native <- fs.tracts.coords(reference_tracks)
  for (candidate in c(plain_file, gz_file)) {
    label <- if (identical(candidate, gz_file)) "our .trk.gz" else "our .trk"
    dump_file <- file.path(work_dir, paste0(basename(candidate), ".dump"))
    result <- nibabel_dump_streamlines(nibabel_python, candidate, dump_file)
    if (result$status != 0L || !file.exists(dump_file)) {
      status <- report(label, "nibabel", "MISMATCH", "nibabel could not read the file")
    } else {
      # nibabel returns the track coordinates in RAS+ mm space, which is what our own reader returns for
      # 'coords = "ras"'. The two are compared for the file at hand, so this validates the writer output and
      # the reader conversion against an independent implementation at the same time.
      ours_ras <- fs.tracts.coords(read.dti.trk(candidate, coords = "ras")$tracks)
      ours_native <- fs.tracts.coords(read.dti.trk(candidate, coords = "native")$tracks)
      nibabel_coords <- read_dump_coords(dump_file)
      nibabel_diff <- max(abs(nibabel_coords - ours_ras))
      native_diff <- max(abs(ours_native - expected_native))
      problems <- character(0L)
      if (!(nibabel_diff < 1e-4)) {
        problems <- c(problems, sprintf("nibabel and this package disagree on the RAS coordinates by %.3g", nibabel_diff))
      }
      if (!(native_diff < 1e-6)) {
        problems <- c(problems, sprintf("the file reads back with coordinates that differ by %.3g", native_diff))
      }
      status <- report(label, "nibabel", if (length(problems) == 0L) "OK" else "MISMATCH",
                       if (length(problems) == 0L) {
                         sprintf("%d points, RAS coordinates and round trip agree", nrow(nibabel_coords))
                       } else {
                         paste(problems, collapse = "; ")
                       })
    }
    result <- tally(status, checks, failures)
    checks <- result$checks
    failures <- result$failures
  }

  # The compressed file really is a gzip file, and nibabel cannot write one itself: this documents why a
  # compressed file has to be produced by compressing an uncompressed one.
  magic_con <- file(gz_file, "rb")
  magic <- readBin(magic_con, "raw", n = 2L)
  close(magic_con)
  gz_ok <- identical(magic, as.raw(c(0x1f, 0x8b)))
  status <- report("gzip magic bytes", "this package", if (gz_ok) "OK" else "MISMATCH",
                   if (gz_ok) "the file starts with 0x1f 0x8b" else "the file is not gzip compressed")
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  nibabel_written <- file.path(work_dir, "nibabel_written.trk.gz")
  cannot_write <- run_python(nibabel_python, sprintf(
    paste(
      "import nibabel as nib",
      "f = nib.streamlines.load(%s)",
      "nib.streamlines.save(f.streamlines, %s)",
      sep = "\n"
    ),
    shQuote(plain_file), shQuote(nibabel_written)
  ))
  cat(sprintf("   (nibabel writing a compressed TRK file: %s)\n",
              if (cannot_write$status != 0L || !file.exists(nibabel_written)) {
                "fails, so compressed files have to be created by compressing an uncompressed one"
              } else {
                "succeeds, so compressed files can also be created with nibabel"
              }))
}

# Part 3: every reader code path handles compressed files -----------------------
cat("=== 3. The streaming code paths on compressed files ===\n")
if (is.null(real_trk_gz)) {
  cat("   SKIPPED: the test data 'extra_test_data/tracts/STR_R.trk.gz' was not found.\n")
  skipped <- skipped + 1L
} else {
  reference <- read.dti.trk(real_trk_gz, coords = "native")$tracks
  reference_coords <- fs.tracts.coords(reference)
  reference_lengths <- fs.tracts.lengths(reference)

  header_ok <- identical(read.dti.trk.header(real_trk_gz), read.dti.trk.header(real_trk))
  status <- report("header reader", "this package", if (header_ok) "OK" else "MISMATCH")
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  num_tracks <- dti.track.count(real_trk_gz)
  status <- report("track counter", "this package", if (identical(num_tracks, length(reference))) "OK" else "MISMATCH",
                   sprintf("%d tracks", num_tracks))
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  bbox_ok <- identical(dti.track.bbox(real_trk_gz), dti.track.bbox(real_trk))
  status <- report("bounding box scanner", "this package", if (bbox_ok) "OK" else "MISMATCH")
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  skipped_tracks <- 5L
  itr <- dti.track.iterator(real_trk_gz, skip_tracks = skipped_tracks)
  iterator_tracks <- list()
  track <- itr$next.track()
  while (!is.null(track)) {
    iterator_tracks <- c(iterator_tracks, list(track))
    track <- itr$next.track()
  }
  itr$close()
  iterator_ok <- length(iterator_tracks) == length(reference) - skipped_tracks
  if (iterator_ok) {
    offset <- sum(reference_lengths[seq_len(skipped_tracks)])
    for (idx in seq_along(iterator_tracks)) {
      num_points <- reference_lengths[skipped_tracks + idx]
      expected_coords <- reference_coords[(offset + 1L):(offset + num_points), , drop = FALSE]
      if (!identical(iterator_tracks[[idx]]$coords, expected_coords)) {
        iterator_ok <- FALSE
        break
      }
      offset <- offset + num_points
    }
  }
  status <- report("track iterator", "this package", if (iterator_ok) "OK" else "MISMATCH",
                   sprintf("%d tracks after skipping %d", length(iterator_tracks), skipped_tracks))
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures
}

cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", checks, failures, skipped))
if (failures > 0L) {
  stop("TRK gzip checks failed.\n")
}
cat("All TRK gzip checks passed.\n")
