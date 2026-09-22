# Cross-validate the ANALYZE 7.5 / NIFTI-1 pair support of freesurferformats against independent implementations:
#
#   Part 1: our reader reads every fixture and is compared against the nibabel reference dump of that fixture
#           (values, header fields, geometry and the SPM interpretation). The dumps are written by
#           dev_tools/generate_analyze_test_data.py.
#   Part 2: nibabel reads the files that THIS package writes with write.analyze() and write.nifti1(), and the
#           values, voxel sizes, data type and (for pairs) the affine are compared.
#   Part 3: FreeSurfer (mri_convert) reads an ANALYZE file written by this package, and we read the ANALYZE file
#           that FreeSurfer wrote from the same volume.
#
# Requires a Python 3 interpreter with nibabel for parts 1 and 2 (set the interpreter with the PYTHON environment
# variable, e.g. PYTHON=~/venv/bin/python), and FreeSurfer for part 3 (set FREESURFER_HOME). Parts whose tools are
# missing are reported as skipped.
#
# Run from the repository root: Rscript dev_tools/check_analyze_conversion.R

suppressMessages(devtools::load_all(".", quiet = TRUE))

test_data_dir <- file.path("extra_test_data", "analyze")
small_data_dir <- file.path("inst", "extdata", "analyze")
expected_dir <- file.path(test_data_dir, "expected")

num_checks <- 0L
num_failures <- 0L
num_skipped <- 0L

report_ok <- function(label) {
  cat(sprintf("  %-46s %s\n", label, "OK"))
}

problem <- function(fmt, ...) {
  num_failures <<- num_failures + 1L
  cat(sprintf("  %-46s FAIL: %s\n", "", sprintf(fmt, ...)))
}


#' Read a reference dump written by the generator script.
#'
#' The dump format is one '<kind> <key> <value>' line per entry, where kind is one of 'header_int',
#' 'header_float', 'header_byte', 'header_intlist', 'header_floatlist', 'header_hex', 'header_str', 'dump' or
#' 'values'.
read_analyze_dump <- function(path) {
  lines <- readLines(path)
  result <- list(header_int = list(), header_float = list(), header_byte = list(), header_intlist = list(),
                 header_floatlist = list(), header_hex = list(), header_str = list(), dump = list(),
                 values = NULL)
  for (line in lines) {
    parts <- strsplit(line, " ", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) {
      next
    }
    kind <- parts[1L]
    key <- parts[2L]
    if (kind == "values") {
      result$values <- as.numeric(parts[-1L])
    } else if (kind %in% names(result)) {
      result[[kind]][[key]] <- if (length(parts) > 2L) paste(parts[-(1:2)], collapse = " ") else ""
    }
  }
  return(result)
}


#' Read the output of dev_tools/nibabel_analyze_dump.py, which is a list of '<key> <value>' lines.
read_named_values <- function(lines) {
  result <- list()
  for (line in lines) {
    parts <- strsplit(trimws(line), " ", fixed = TRUE)[[1L]]
    if (length(parts) >= 2L) {
      result[[parts[1L]]] <- paste(parts[-1L], collapse = " ")
    }
  }
  return(result)
}


numeric_equal <- function(actual, expected, tolerance = 1e-6) {
  if (length(actual) != length(expected)) {
    return(FALSE)
  }
  if (any(is.na(actual)) || any(is.na(expected))) {
    return(all(is.na(actual)) && all(is.na(expected)))
  }
  return(max(abs(actual - expected)) <= tolerance)
}


#' Render a string the way the generator does it, for comparison with a dump string field.
printable_string <- function(value) {
  bytes <- as.integer(charToRaw(value))
  if (length(bytes) == 0L) {
    return("")
  }
  chars <- ifelse(bytes >= 32L & bytes < 127L, rawToChar(as.raw(bytes), multiple = TRUE), ".")
  return(sub("[\\.]+$", "", paste(chars, collapse = "")))
}


#' Compare one fixture against its nibabel reference dump. Returns TRUE if all checks passed.
check_fixture <- function(fixture, dump, is_nifti_pair) {
  ok <- TRUE

  # --- values and dimensions, read through the volume reader, which dispatches on the header magic ---
  vol <- suppressWarnings(read.fs.volume.analyze(fixture, with_header = TRUE))
  values <- as.numeric(as.vector(vol$data))
  if (length(values) != length(dump$values)) {
    problem("'%s': we read %d values, nibabel reports %d", basename(fixture), length(values), length(dump$values))
    return(FALSE)
  }
  if (!numeric_equal(values, dump$values, tolerance = 0)) {
    problem("'%s': values differ from nibabel by up to %.4g", basename(fixture), max(abs(values - dump$values)))
    ok <- FALSE
  }

  # --- the header fields, as they are stored in the file ---
  header <- vol$header
  for (fieldname in names(dump$header_int)) {
    expected <- as.integer(dump$header_int[[fieldname]])
    if (fieldname == "bitpix" && !is.null(dump$dump$file_bitpix)) {
      expected <- as.integer(dump$dump$file_bitpix) # the file contradicts itself here, see the generator
    }
    if (!identical(as.integer(header[[fieldname]]), expected)) {
      problem("'%s': header field '%s' is %s, expected %s", basename(fixture), fieldname, header[[fieldname]], expected)
      ok <- FALSE
    }
  }
  for (fieldname in names(dump$header_float)) {
    if (!numeric_equal(as.numeric(header[[fieldname]]), as.numeric(dump$header_float[[fieldname]]), tolerance = 1e-9)) {
      problem("'%s': header field '%s' is %s, expected %s", basename(fixture), fieldname, header[[fieldname]],
              dump$header_float[[fieldname]])
      ok <- FALSE
    }
  }
  for (fieldname in names(dump$header_byte)) {
    if (is.null(header[[fieldname]])) {
      problem("'%s': header field '%s' is missing", basename(fixture), fieldname)
      ok <- FALSE
    } else if (!identical(as.integer(header[[fieldname]]), as.integer(dump$header_byte[[fieldname]]))) {
      problem("'%s': header field '%s' is %d, expected %d", basename(fixture), fieldname, header[[fieldname]],
              as.integer(dump$header_byte[[fieldname]]))
      ok <- FALSE
    }
  }
  for (fieldname in names(dump$header_intlist)) {
    our_field <- if (fieldname == "dim") "dim" else "pix_dim"
    expected <- as.numeric(strsplit(dump$header_intlist[[fieldname]], " ")[[1L]])
    if (!numeric_equal(as.numeric(header[[our_field]]), expected, tolerance = 0)) {
      problem("'%s': header field '%s' is (%s), expected (%s)", basename(fixture), fieldname,
              paste(header[[our_field]], collapse = " "), paste(expected, collapse = " "))
      ok <- FALSE
    }
  }
  if (!is.null(dump$header_floatlist$pix_dim)) {
    expected <- as.numeric(strsplit(dump$header_floatlist$pix_dim, " ")[[1L]])
    if (!numeric_equal(as.numeric(header$pix_dim), expected, tolerance = 1e-6)) {
      problem("'%s': 'pix_dim' is (%s), expected (%s)", basename(fixture),
              paste(header$pix_dim, collapse = " "), paste(expected, collapse = " "))
      ok <- FALSE
    }
  }
  # The character fields: the bytes (from the hex dump, without the trailing zero padding) and the printable form.
  for (fieldname in names(dump$header_hex)) {
    if (is.null(header[[fieldname]])) {
      problem("'%s': header field '%s' is missing", basename(fixture), fieldname)
      ok <- FALSE
      next
    }
    expected_hex <- gsub("00", "", dump$header_hex[[fieldname]]) # drop the zero bytes, our strings omit them
    raw_bytes <- charToRaw(header[[fieldname]])
    actual_hex <- if (length(raw_bytes) > 0L) paste(sprintf("%02x", as.integer(raw_bytes)), collapse = "") else ""
    if (!identical(actual_hex, expected_hex)) {
      problem("'%s': the bytes of field '%s' are '%s', expected '%s'", basename(fixture), fieldname, actual_hex, expected_hex)
      ok <- FALSE
    }
    if (fieldname %in% names(dump$header_str) && !identical(printable_string(header[[fieldname]]), dump$header_str[[fieldname]])) {
      problem("'%s': the string of field '%s' is '%s', expected '%s'", basename(fixture), fieldname,
              printable_string(header[[fieldname]]), dump$header_str[[fieldname]])
      ok <- FALSE
    }
  }

  # --- the geometry ---
  # Note that the dumps store the matrices in row-major order (the order in which nibabel, numpy and the format
  # specifications list them), while an R matrix is stored column-major, hence the transpose before comparison.
  if (is_nifti_pair) {
    # A NIFTI pair stores a proper affine (an sform or a qform), which has to match nibabel's exactly.
    expected_affine <- as.numeric(strsplit(dump$dump$nibabel_affine, " ")[[1L]])
    if (is.null(vol$header$vox2ras_matrix)) {
      problem("'%s': we report no vox2ras matrix, nibabel reports one", basename(fixture))
      ok <- FALSE
    } else if (!numeric_equal(as.numeric(t(vol$header$vox2ras_matrix)), expected_affine, tolerance = 1e-5)) {
      problem("'%s': the vox2ras matrix differs from nibabel by up to %.4g", basename(fixture),
              max(abs(as.numeric(t(vol$header$vox2ras_matrix)) - expected_affine)))
      ok <- FALSE
    } else if (is.null(vol$header$vox2ras_source)) {
      problem("'%s': the vox2ras matrix has no documented source", basename(fixture))
      ok <- FALSE
    }
  } else {
    # ANALYZE stores no affine in its header. If the file has a MATLAB sidecar with the matrix, that matrix is
    # used (and it has to match nibabel's); otherwise the default must not invent one.
    has_sidecar <- !is.null(dump$dump$mat_sidecar) && nzchar(trimws(dump$dump$mat_sidecar))
    expected_affine <- as.numeric(strsplit(dump$dump$nibabel_affine, " ")[[1L]])
    if (has_sidecar) {
      if (is.null(vol$header$vox2ras_matrix)) {
        problem("'%s': the MATLAB sidecar file was not used", basename(fixture))
        ok <- FALSE
      } else if (!numeric_equal(as.numeric(t(vol$header$vox2ras_matrix)), expected_affine, tolerance = 1e-5)) {
        problem("'%s': the matrix from the MATLAB sidecar differs from nibabel by up to %.4g", basename(fixture),
                max(abs(as.numeric(t(vol$header$vox2ras_matrix)) - expected_affine)))
        ok <- FALSE
      } else if (!identical(vol$header$vox2ras_source, "mat sidecar")) {
        problem("'%s': the matrix reports source '%s' instead of 'mat sidecar'", basename(fixture), vol$header$vox2ras_source)
        ok <- FALSE
      }
    } else if (!is.null(vol$header$vox2ras_matrix)) {
      problem("'%s': we report a vox2ras matrix although ANALYZE files store none (spm = FALSE)", basename(fixture))
      ok <- FALSE
    }
    spm_vol <- suppressWarnings(read.fs.volume.analyze(fixture, with_header = TRUE, spm = TRUE))
    # For files with a MATLAB sidecar, nibabel's affine comes from that sidecar, so there is only one reference.
    spm_reference <- if (!is.null(dump$dump$nibabel_spm_affine)) dump$dump$nibabel_spm_affine else dump$dump$nibabel_affine
    expected_spm_affine <- as.numeric(strsplit(spm_reference, " ")[[1L]])
    actual_spm_affine <- as.numeric(t(spm_vol$header$vox2ras_matrix))
    if (!numeric_equal(actual_spm_affine, expected_spm_affine, tolerance = 1e-5)) {
      problem("'%s': the SPM affine (%s) differs from nibabel (%s)", basename(fixture),
              paste(round(actual_spm_affine, 3L), collapse = " "), paste(round(expected_spm_affine, 3L), collapse = " "))
      ok <- FALSE
    }
    # The SPM scale factor is applied when requested, and never otherwise.
    unscaled <- as.numeric(as.vector(vol$data))
    scaled <- as.numeric(as.vector(spm_vol$data))
    slope <- as.numeric(header$funused1)
    expected_scaled <- if (slope != 0. && slope != 1.) unscaled * slope else unscaled
    if (!numeric_equal(scaled, expected_scaled, tolerance = 1e-6)) {
      problem("'%s': the SPM scale factor (%.4f) was not applied correctly", basename(fixture), slope)
      ok <- FALSE
    }
    # A file that uses the SPM fields has to warn about them when they are not requested. A MATLAB sidecar file
    # supersedes the origin, so an origin alone does not have to be reported then.
    spm_fields_used <- (!is.null(header$spm_origin) && !has_sidecar) || (header$funused1 != 0. && header$funused1 != 1.)
    warned <- FALSE
    withCallingHandlers(read.fs.volume.analyze(fixture),
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
    if (spm_fields_used && !warned) {
      problem("'%s': reading did not warn about the SPM fields that the file uses", basename(fixture))
      ok <- FALSE
    }
  }

  return(ok)
}


run_python <- function(args) {
  python <- Sys.getenv("PYTHON", unset = "python3")
  # system2 does not quote its arguments, and the code passed to '-c' contains spaces.
  return(system2(path.expand(python), shQuote(args), stdout = TRUE, stderr = TRUE))
}


nibabel_available <- function() {
  out <- suppressWarnings(run_python(c("-c", "import nibabel")))
  status <- attr(out, "status")
  return(is.null(status) || status == 0L)
}


cat("Checking ANALYZE 7.5 / NIFTI v1 pair support of freesurferformats.\n")

# ------------------------------------------------------------------------------------------------------------
cat("\nPart 1: reading the fixtures, compared against the nibabel reference dumps.\n")
# ------------------------------------------------------------------------------------------------------------
fixtures <- unique(c(
  list.files(test_data_dir, pattern = "\\.hdr$|\\.hdr\\.gz$", full.names = TRUE),
  list.files(small_data_dir, pattern = "\\.hdr$|\\.hdr\\.gz$", full.names = TRUE)
))
# The same fixtures exist twice (once shipped with the package, once as dev test data), so keep the first one per
# file name. Also, the ANALYZE file written by FreeSurfer has no nibabel dump, it is checked in part 3.
fixtures <- fixtures[!duplicated(basename(fixtures))]
fixtures <- fixtures[basename(fixtures) != "fs_tiny.hdr"]
for (fixture in fixtures) {
  num_checks <- num_checks + 1L
  dump_file <- file.path(expected_dir, sprintf("%s.expected.txt", basename(fixture)))
  if (!file.exists(dump_file)) {
    problem("fixture '%s' has no reference dump, regenerate the test data", basename(fixture))
    next
  }
  dump <- read_analyze_dump(dump_file)
  # The generator records the class that nibabel picked when loading the file ('Nifti1Pair' for a pair).
  is_nifti_pair <- identical(trimws(dump$dump$nibabel_class), "Nifti1Pair")
  if (check_fixture(fixture, dump, is_nifti_pair)) {
    report_ok(basename(fixture))
  }
}

# Every dump needs a fixture, so that removing a fixture without regenerating the data is caught.
num_checks <- num_checks + 1L
dumps_without_fixture <- setdiff(sub("\\.expected\\.txt$", "", list.files(expected_dir)), basename(fixtures))
if (length(dumps_without_fixture) > 0L) {
  problem("reference dumps without a fixture: %s", paste(dumps_without_fixture, collapse = ", "))
} else {
  report_ok("every dump has a fixture")
}

# ------------------------------------------------------------------------------------------------------------
cat("\nPart 2: nibabel reads what we write.\n")
# ------------------------------------------------------------------------------------------------------------
num_checks <- num_checks + 1L
if (!nibabel_available()) {
  cat("  nibabel is not available, skipping. Set the PYTHON environment variable to an interpreter with nibabel.\n")
  num_skipped <- num_skipped + 1L
} else {
  dumper <- file.path("dev_tools", "nibabel_analyze_dump.py")
  check_data <- array(as.integer(read.analyze.data(file.path(small_data_dir, "tiny_i16.hdr"))), dim = c(4L, 3L, 2L))

  # --- an ANALYZE file written by us ---
  analyze_out <- tempfile()
  analyzeheader <- analyzeheader.for.data(check_data)
  analyzeheader$pix_dim <- c(1., 1.5, 1.5, 2., 0., 0., 0., 0.)
  analyzeheader$orient <- 3L
  analyzeheader$descrip <- "written by freesurferformats"
  write.analyze(analyze_out, check_data, analyzeheader)
  dump <- read_named_values(run_python(c(dumper, paste0(analyze_out, ".hdr"))))
  num_checks <- num_checks + 1L
  if (!identical(as.integer(dump$num_values), length(check_data)) || !identical(as.numeric(dump$sum), as.numeric(sum(check_data)))) {
    problem("nibabel reads the ANALYZE file we wrote as %s values with sum %s, expected %d values with sum %d",
            dump$num_values, dump$sum, length(check_data), sum(check_data))
  } else {
    report_ok("nibabel reads our ANALYZE file (values)")
  }
  num_checks <- num_checks + 1L
  if (!numeric_equal(as.numeric(strsplit(dump$pixdim, " ")[[1L]][2:4]), c(1.5, 1.5, 2.), tolerance = 1e-6)) {
    problem("nibabel reports voxel sizes '%s' for our ANALYZE file, expected 1.5 1.5 2", dump$pixdim)
  } else {
    report_ok("nibabel reads our ANALYZE file (voxel sizes)")
  }
  num_checks <- num_checks + 1L
  if (!(trimws(dump$dtype) %in% c("int16", "int32"))) {
    problem("nibabel reports data type '%s' for our ANALYZE file", dump$dtype)
  } else {
    report_ok("nibabel reads our ANALYZE file (data type)")
  }

  # --- a NIFTI v1 pair written by us ---
  pair_out <- tempfile()
  pair_affine <- diag(4L)
  pair_affine[1:3, 1:3] <- matrix(c(-1., 0., 0., 0., 0., 1., 0., -1., 0.), nrow = 3L, byrow = TRUE)
  pair_affine[1:3, 4L] <- c(11., 22., 33.)
  pair_header <- ni1header.template(pair = TRUE)
  pair_header$datatype <- 4L
  pair_header$bitpix <- 16L
  pair_header$dim <- nifti.datadim.to.dimfield(c(4L, 3L, 2L))
  pair_header$sform_code <- 1L
  pair_header$srow_x <- pair_affine[1L, ]
  pair_header$srow_y <- pair_affine[2L, ]
  pair_header$srow_z <- pair_affine[3L, ]
  write.nifti1(pair_out, check_data, pair_header)
  dump <- read_named_values(run_python(c(dumper, paste0(pair_out, ".hdr"))))
  num_checks <- num_checks + 1L
  if (!identical(trimws(dump$loaded_class), "Nifti1Pair") || !identical(as.numeric(dump$sum), as.numeric(sum(check_data)))) {
    problem("nibabel reads our pair as '%s' with sum %s, expected Nifti1Pair with sum %d", dump$loaded_class, dump$sum, sum(check_data))
  } else {
    report_ok("nibabel reads our NIFTI v1 pair (values)")
  }
  num_checks <- num_checks + 1L
  if (!numeric_equal(as.numeric(strsplit(dump$affine, " ")[[1L]]), as.numeric(t(pair_affine)), tolerance = 1e-5)) {
    problem("nibabel reports a different affine for our NIFTI pair: %s", dump$affine)
  } else {
    report_ok("nibabel reads our NIFTI v1 pair (affine)")
  }

  # --- a compressed pair written by us ---
  gz_out <- tempfile()
  write.nifti1(paste0(gz_out, ".hdr.gz"), check_data, pair_header)
  dump <- read_named_values(run_python(c(dumper, paste0(gz_out, ".hdr.gz"))))
  num_checks <- num_checks + 1L
  if (!identical(as.numeric(dump$sum), as.numeric(sum(check_data)))) {
    problem("nibabel reads our compressed pair with sum %s, expected %d", dump$sum, sum(check_data))
  } else {
    report_ok("nibabel reads our compressed pair")
  }
}

# ------------------------------------------------------------------------------------------------------------
cat("\nPart 3: FreeSurfer interoperation.\n")
# ------------------------------------------------------------------------------------------------------------
num_checks <- num_checks + 1L
mri_convert <- file.path(Sys.getenv("FREESURFER_HOME", unset = "."), "bin", "mri_convert")
if (!nzchar(Sys.getenv("FREESURFER_HOME", unset = "")) || !file.exists(mri_convert)) {
  cat("  FreeSurfer is not available (FREESURFER_HOME is not set or mri_convert is missing), skipping.\n")
  num_skipped <- num_skipped + 1L
} else {
  tiny_mgh <- system.file("extdata", "tiny.mgh", package = "freesurferformats", mustWork = TRUE)
  tiny_vol <- read.fs.mgh(tiny_mgh)

  # --- our ANALYZE file, read back by FreeSurfer ---
  fs_out <- tempfile()
  fs_header <- analyzeheader.for.data(tiny_vol)
  fs_header$pix_dim <- c(1., 1., 1., 1., 0., 0., 0., 0.)
  fs_header$datatype <- 8L
  fs_header$bitpix <- 32L
  write.analyze(fs_out, tiny_vol, fs_header)
  back_mgh <- tempfile(fileext = ".mgz")
  # FreeSurfer does not infer the ANALYZE format from the '.hdr' file extension and wants the data file, so the
  # input type has to be given and the '.img' file has to be passed.
  conversion_log <- suppressWarnings(system2(mri_convert, c("-it", "analyze", "-ot", "mgh", paste0(fs_out, ".img"), back_mgh),
                                             stdout = TRUE, stderr = TRUE))
  num_checks <- num_checks + 1L
  if (!file.exists(back_mgh)) {
    problem("FreeSurfer could not convert the ANALYZE file we wrote: %s", paste(tail(conversion_log, 3L), collapse = " "))
  } else if (!numeric_equal(as.numeric(read.fs.mgh(back_mgh)), as.numeric(tiny_vol), tolerance = 0)) {
    problem("the values FreeSurfer read from our ANALYZE file differ from the volume we wrote")
  } else {
    report_ok("FreeSurfer reads our ANALYZE file (values)")
  }

  # --- the ANALYZE file that FreeSurfer wrote, read by us ---
  fs_fixture <- file.path(test_data_dir, "fs_tiny.hdr")
  num_checks <- num_checks + 1L
  if (!file.exists(fs_fixture)) {
    problem("the FreeSurfer-written fixture '%s' is missing, see the generator script", fs_fixture)
  } else {
    fs_vol <- suppressWarnings(read.fs.volume.analyze(fs_fixture, with_header = TRUE, spm = TRUE))
    if (!numeric_equal(as.numeric(fs_vol$data), as.numeric(tiny_vol), tolerance = 0)) {
      problem("the values we read from the FreeSurfer-written ANALYZE file differ from 'tiny.mgh'")
    } else if (is.null(fs_vol$header$spm_origin)) {
      problem("we did not find the SPM origin in the FreeSurfer-written ANALYZE file")
    } else {
      report_ok("we read the FreeSurfer-written ANALYZE file")
    }

    # The file that FreeSurfer wrote has a MATLAB sidecar with the transformation matrix, and it has to describe
    # the same geometry that nibabel reads from that file.
    num_checks <- num_checks + 1L
    if (nibabel_available()) {
      nibabel_dump <- read_named_values(run_python(c(file.path("dev_tools", "nibabel_analyze_dump.py"), fs_fixture)))
      fs_default <- suppressWarnings(read.fs.volume.analyze(fs_fixture, with_header = TRUE))
      expected_affine <- as.numeric(strsplit(nibabel_dump$affine, " ")[[1L]])
      if (is.null(fs_default$header$vox2ras_matrix)) {
        problem("the MATLAB sidecar file of the FreeSurfer-written ANALYZE file was not used")
      } else if (!numeric_equal(as.numeric(t(fs_default$header$vox2ras_matrix)), expected_affine, tolerance = 1e-5)) {
        problem("the matrix from the sidecar of the FreeSurfer-written file differs from nibabel by up to %.4g",
                max(abs(as.numeric(t(fs_default$header$vox2ras_matrix)) - expected_affine)))
      } else {
        report_ok("the sidecar of the FreeSurfer file matches nibabel")
      }
    } else {
      num_skipped <- num_skipped + 1L
    }
  }
}

cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", num_checks, num_failures, num_skipped))
if (num_failures == 0L) {
  cat("All ANALYZE conversion checks passed.\n")
} else {
  cat("Some ANALYZE conversion checks FAILED, see the output above.\n")
}
