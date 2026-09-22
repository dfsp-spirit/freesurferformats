# Cross-validate the TSF (track scalar file) support of this package against MRtrix.
#
# A TSF file stores one scalar value per streamline point and no track boundaries, so it can only be validated
# against the tractogram it belongs to. MRtrix ships 'tsfvalidate <scalars> <tracks>' for exactly that, and
# this script uses it as the reference implementation. It checks the following:
#
#   1. Files written by this package are accepted by MRtrix: 'tsfinfo' parses their header, and 'tsfvalidate'
#      confirms that their values are split into exactly the tracks of a TCK file written by this package for
#      the same streamlines. This is done for all 4 supported datatypes.
#   2. Files written by MRtrix ('tsfmult') are read back with the values MRtrix wrote.
#   3. The real-world whole-brain tractogram of the package (opt data, in TCK and TSF format) is used when it
#      is available: the first tracks are read, written back out with this package and validated by MRtrix.
#
# The MRtrix parts are optional: the script reports which of them it could not find and skips those checks.
# Run it with
#   Rscript dev_tools/check_mrtrix_tsf.R
# and set MRTRIX_BIN to the directory containing the MRtrix binaries if they are not in the default location.

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
repo_root <- if (length(script_file) == 1L) normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- getwd()
}
devtools::load_all(repo_root, quiet = TRUE)

find_mrtrix_bin <- function() {
  candidates <- c(
    Sys.getenv("MRTRIX_BIN", unset = NA),
    path.expand("~/software/micromamba/envs/mrtrix3/bin"),
    dirname(Sys.which("tsfinfo"))
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "tsfvalidate"))) {
      return(candidate)
    }
  }
  return(NULL)
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
  cat(sprintf("   [%-22s] %-34s : %-8s %s\n", check_name, tool_label, status, detail))
  return(status)
}

checks <- 0L
failures <- 0L
skipped <- 0L

mrtrix_bin <- find_mrtrix_bin()
if (is.null(mrtrix_bin)) {
  cat("SKIPPED: the MRtrix3 tools were not found. Install MRtrix3 and/or set the MRTRIX_BIN environment\n")
  cat("variable to the directory that contains them (the tools 'tsfinfo' and 'tsfvalidate' are required).\n")
  quit(status = 0L)
}
cat(sprintf("Using MRtrix3 tools from '%s'.\n", mrtrix_bin))

run_mrtrix <- function(tool, args) {
  output <- suppressWarnings(system2(file.path(mrtrix_bin, tool), args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  return(list(status = status, output = paste(output, collapse = "\n")))
}

# The first non-empty line that mentions a problem, for the report.
mrtrix_error_line <- function(output) {
  lines <- trimws(strsplit(output, "\n")[[1L]])
  lines <- lines[nzchar(lines)]
  problem <- lines[grepl("ERROR|error", lines)]
  if (length(problem) > 0L) {
    return(sub("^.*ERROR\\] ", "", problem[1L]))
  }
  if (length(lines) > 0L) {
    return(lines[length(lines)])
  }
  return("no output")
}

work_dir <- tempfile("mrtrix_tsf_check_")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

# Part 1: this package writes, MRtrix validates ---------------------------------
cat("=== 1. Files written by this package, validated by MRtrix ===\n")

values <- list(c(1, 2, 3, 4), c(5, 6, 7), c(8, 9, 10))
# A TCK file with the same number of tracks and the same number of points per track has to exist for the
# validation: the track boundaries are stored in that file, not in the TSF file.
tracks <- lapply(values, function(track_values) cbind(seq_along(track_values), 0, 0))

for (datatype in c("Float32LE", "Float32BE", "Float64LE", "Float64BE")) {
  tsf_file <- file.path(work_dir, sprintf("scalars_%s.tsf", datatype))
  tck_file <- file.path(work_dir, sprintf("tracks_%s.tck", datatype))
  write.dti.tsf(values, tsf_file, datatype = datatype)
  write.dti.tck(tracks, tck_file, datatype = datatype)

  info <- run_mrtrix("tsfinfo", tsf_file)
  validation <- run_mrtrix("tsfvalidate", c(tsf_file, tck_file))
  ok <- info$status == 0L && validation$status == 0L && grepl("checked OK", validation$output)
  status <- report(sprintf("tsf, %s", datatype), "MRtrix tsfvalidate", if (ok) "OK" else "MISMATCH",
                   if (ok) "values and track boundaries accepted" else mrtrix_error_line(validation$output))
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures
}

# A file whose header states a different number of tracks than its delimiters define must be rejected: this
# confirms that 'tsfvalidate' really inspects the file, and hence that the checks above are not vacuous.
broken_tsf <- file.path(work_dir, "broken_count.tsf")
{
  values_broken <- c(1, 2, 3, 4, NaN, 5, 6, 7, NaN, 8, 9, 10, NaN)
  offset <- 100L
  repeat {
    lines <- c("mrtrix track scalars", "datatype: Float32LE", "count: 4", sprintf("file: . %d", offset), "END")
    header <- paste0(paste(lines, collapse = "\n"), "\n")
    new_offset <- nchar(header, type = "bytes")
    if (new_offset == offset) {
      break
    }
    offset <- new_offset
  }
  con <- file(broken_tsf, "wb")
  writeBin(charToRaw(header), con)
  writeBin(as.numeric(values_broken), con, size = 4L, endian = "little")
  close(con)
}
broken <- run_mrtrix("tsfvalidate", c(broken_tsf, file.path(work_dir, "tracks_Float32LE.tck")))
rejected <- !grepl("checked OK", broken$output)
status <- report("wrong track count", "MRtrix tsfvalidate", if (rejected) "OK" else "MISMATCH",
                 if (rejected) "rejected as expected" else "accepted a broken file")
result <- tally(status, checks, failures)
checks <- result$checks
failures <- result$failures

# Part 2: MRtrix writes, this package reads -------------------------------------
cat("=== 2. Files written by MRtrix, read by this package ===\n")

input_tsf <- file.path(work_dir, "input.tsf")
# The pairing tools refuse to combine files whose 'timestamp' header entries differ, since a mismatch means
# that the files describe different tractograms. Writing the entry is also a check that additional header
# entries survive the round trip through this package.
write.dti.tsf(values, input_tsf, header = list(timestamp = "12345.0"))
squared_tsf <- file.path(work_dir, "squared.tsf")
# 'tsfmult' multiplies two track scalar files, so squaring a file means passing it twice.
multiply <- run_mrtrix("tsfmult", c(input_tsf, input_tsf, squared_tsf, "-force"))

if (multiply$status != 0L || !file.exists(squared_tsf)) {
  status <- report("tsfmult output", "MRtrix tsfmult", "MISMATCH", mrtrix_error_line(multiply$output))
} else {
  read_back <- read.dti.tsf(squared_tsf)$scalars
  expected <- unlist(values)^2
  lengths_ok <- identical(as.integer(read_back$lengths), as.integer(vapply(values, length, integer(1L))))
  value_diff <- if (length(read_back$merged) == length(expected)) max(abs(read_back$merged - expected)) else Inf
  problems <- character(0L)
  if (!lengths_ok) {
    problems <- c(problems, "track lengths differ")
  }
  if (!(value_diff < 1e-5)) {
    problems <- c(problems, sprintf("values differ by %.3g", value_diff))
  }
  status <- report("tsfmult output", "MRtrix tsfmult", if (length(problems) == 0L) "OK" else "MISMATCH",
                   if (length(problems) == 0L) sprintf("max value diff %.3g", value_diff) else paste(problems, collapse = "; "))
}
result <- tally(status, checks, failures)
checks <- result$checks
failures <- result$failures

# Part 3: the real-world tractogram of the package ------------------------------
cat("=== 3. Real-world data (optional) ===\n")

find_opt_data <- function(relpath) {
  candidates <- c(
    path.expand(file.path("~/develop/nitestdata", relpath)),
    file.path(repo_root, "extra_test_data", relpath)
  )
  cached <- tryCatch(freesurferformats::get_opt_data_filepath(relpath, mustWork = FALSE), error = function(e) NULL)
  if (!is.null(cached) && length(cached) == 1L) {
    candidates <- c(candidates, cached)
  }
  for (candidate in candidates) {
    if (is.character(candidate) && length(candidate) == 1L && file.exists(candidate)) {
      return(candidate)
    }
  }
  return(NULL)
}

real_tck <- find_opt_data(file.path("dwi", "tracks.tck"))
real_tsf <- find_opt_data(file.path("dwi", "tracks.tsf"))

if (is.null(real_tck) || is.null(real_tsf)) {
  cat("   SKIPPED: the optional whole-brain tractogram ('dwi/tracks.tck' and 'dwi/tracks.tsf') was not found.\n")
  cat("   Download it with freesurferformats::download_opt_data().\n")
  skipped <- skipped + 1L
} else {
  max_tracks <- 20L
  real_tck_subset <- read.dti.tck(real_tck, max_tracks = max_tracks)
  real_tsf_subset <- read.dti.tsf(real_tsf, max_tracks = max_tracks)
  lengths_match <- identical(as.integer(real_tsf_subset$scalars$lengths),
                             as.integer(fs.tracts.lengths(real_tck_subset$tracks)))

  rewritten_tck <- file.path(work_dir, "real_subset.tck")
  rewritten_tsf <- file.path(work_dir, "real_subset.tsf")
  write.dti.tck(real_tck_subset$tracks, rewritten_tck, header = real_tck_subset$header)
  write.dti.tsf(real_tsf_subset$scalars, rewritten_tsf, header = real_tsf_subset$header)

  validation <- run_mrtrix("tsfvalidate", c(rewritten_tsf, rewritten_tck))
  ok <- lengths_match && validation$status == 0L && grepl("checked OK", validation$output)
  detail <- if (ok) {
    sprintf("%d tracks, %d points, re-written and accepted", max_tracks, sum(real_tsf_subset$scalars$lengths))
  } else if (!lengths_match) {
    "the lengths of the TCK and the TSF file differ"
  } else {
    mrtrix_error_line(validation$output)
  }
  status <- report("real tractogram subset", "MRtrix tsfvalidate", if (ok) "OK" else "MISMATCH", detail)
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  # The payload of the file written by this package must be byte-identical to the part of the original file
  # that it describes: same values, same datatype, and the same NaN delimiter after every track. This compares
  # against a file written by MRtrix itself (the opt data was produced by 'tcksample'), not against our own
  # expectations of the layout.
  plain_header <- read.dti.tsf.header(real_tsf)
  written_header <- read.dti.tsf.header(rewritten_tsf)
  payload_bytes <- (sum(real_tsf_subset$scalars$lengths) + max_tracks) * plain_header$derived$dsize
  same_datatype <- identical(plain_header$datatype, written_header$datatype)
  if (!same_datatype) {
    status <- report("payload bytes", "MRtrix-tcksample file", "MISMATCH", "the datatypes of the files differ")
  } else {
    original_bytes <- readBin(real_tsf, "raw", n = plain_header$derived$data_offset + payload_bytes)
    written_bytes <- readBin(rewritten_tsf, "raw", n = file.size(rewritten_tsf))
    identical_payload <- identical(original_bytes[(plain_header$derived$data_offset + 1L):length(original_bytes)],
                                   written_bytes[(written_header$derived$data_offset + 1L):length(written_bytes)])
    status <- report("payload bytes", "MRtrix-tcksample file",
                     if (identical_payload) "OK" else "MISMATCH",
                     if (identical_payload) sprintf("%d bytes identical to the original", payload_bytes) else "the payload bytes differ")
  }
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures
}

cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", checks, failures, skipped))
if (failures > 0L) {
  stop("MRtrix TSF checks failed.\n")
}
cat("All MRtrix TSF checks passed.\n")
