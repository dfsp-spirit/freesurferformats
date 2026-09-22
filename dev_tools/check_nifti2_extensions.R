# Cross-validate the NIFTI v2 header extension support of freesurferformats against independent implementations:
#
#   Part 1: nibabel reads the NIFTI v2 files that THIS package writes (with and without header extensions, text
#           and binary payloads, compressed and uncompressed), and the byte layout of the extension area, the
#           data offset, the extension payloads and the voxel data are compared.
#   Part 2: nibabel writes a NIFTI v2 file with two known header extensions, and our reader must report exactly
#           the same extensions, data offset and voxel data.
#   Part 3: the header extensions of the CIFTI-2 example files written by Connectome Workbench are read by our
#           reader and compared byte for byte with what nibabel reports (the XML metadata), and Connectome
#           Workbench is asked to read a NIFTI v2 file written by this package.
#
# Requires a Python 3 interpreter with nibabel (set the interpreter with the PYTHON environment variable, e.g.
# PYTHON=~/venv/bin/python) and, for part 3, Connectome Workbench (set WB_COMMAND). Parts whose tools are
# missing are reported as skipped.
#
# Run from the repository root: Rscript dev_tools/check_nifti2_extensions.R

suppressMessages(devtools::load_all(".", quiet = TRUE))

num_checks <- 0L
num_failures <- 0L
num_skipped <- 0L

report_ok <- function(label) {
  num_checks <<- num_checks + 1L
  cat(sprintf("  %-46s %s\n", label, "OK"))
}

report_skip <- function(label, reason) {
  num_skipped <<- num_skipped + 1L
  cat(sprintf("  %-46s SKIP: %s\n", label, reason))
}

problem <- function(fmt, ...) {
  num_checks <<- num_checks + 1L
  num_failures <<- num_failures + 1L
  cat(sprintf("  %-46s FAIL: %s\n", "", sprintf(fmt, ...)))
}

#' Run a python script and return its output lines. system2 does not quote its arguments, so single arguments
#' that contain spaces (like inline code) must be quoted by the caller.
run_python <- function(args) {
  python <- Sys.getenv("PYTHON", unset = "python3")
  return(system2(path.expand(python), args, stdout = TRUE, stderr = TRUE))
}

python_has_nibabel <- function() {
  out <- suppressWarnings(run_python(c("-c", shQuote("import nibabel"))))
  status <- attr(out, "status")
  return(is.null(status) || status == 0L)
}

#' Read the '<key> <value>' output of dev_tools/nibabel_nifti2_dump.py.
read_named_values <- function(lines) {
  result <- list()
  for (line in lines) {
    parts <- strsplit(trimws(line), " ", fixed = TRUE)[[1L]]
    if (length(parts) >= 2L && parts[1L] != "extension") {
      result[[parts[1L]]] <- paste(parts[-1L], collapse = " ")
    }
  }
  return(result)
}

#' The 'extension <index> <ecode> <size_on_disk> <content_length>' lines of the dump.
read_dump_extensions <- function(lines) {
  result <- list()
  for (line in lines) {
    parts <- strsplit(trimws(line), " ", fixed = TRUE)[[1L]]
    if (length(parts) == 5L && parts[1L] == "extension") {
      result[[length(result) + 1L]] <- list("index" = as.integer(parts[2L]), "ecode" = as.integer(parts[3L]),
                                           "size" = as.integer(parts[4L]), "content_length" = as.integer(parts[5L]))
    }
  }
  return(result)
}

num_value <- function(dump, key) {
  if (is.null(dump[[key]])) {
    return(NA)
  }
  return(as.numeric(dump[[key]]))
}

str_value <- function(dump, key) {
  if (is.null(dump[[key]])) {
    return(NA_character_)
  }
  return(as.character(dump[[key]]))
}

cat("Checking the NIFTI v2 header extension support of freesurferformats.\n\n")

work_dir <- tempfile("nifti2_check")
dir.create(work_dir, recursive = TRUE)

dump_file <- function(path, payload_dir = NULL) {
  dumper <- file.path("dev_tools", "nibabel_nifti2_dump.py")
  args <- c(dumper, "dump", path)
  if (!is.null(payload_dir)) {
    args <- c(args, payload_dir)
  }
  return(run_python(args))
}

# =====================================================================================
cat("Part 1: nibabel reads the NIFTI v2 files we write.\n")
# =====================================================================================

if (!python_has_nibabel()) {
  report_skip("all part 1 and 2 checks", "nibabel is not available. Set the PYTHON environment variable.")
} else {
  niidata <- array(as.double(1:24), dim = c(4, 3, 2))
  text_payload <- "a comment stored in a header extension"
  raw_payload <- as.raw(1:9)
  ext_text <- nifti2.extension(6L, text_payload)
  ext_raw <- nifti2.extension(4L, raw_payload)

  our_file <- file.path(work_dir, "ours_with_ext.nii")
  write.nifti2(our_file, niidata, extensions = list(ext_text, ext_raw))
  payload_dir <- file.path(work_dir, "payloads_ours")
  lines <- dump_file(our_file, payload_dir)
  dump <- read_named_values(lines)
  dump_ext <- read_dump_extensions(lines)

  file_bytes <- readBin(our_file, "raw", n = 552L)

  # The magic string and the extension flag bytes, straight from the file.
  if (identical(str_value(dump, "raw_magic"), "6e2b32000d0a1a0a")) {
    report_ok("nibabel reports the NIFTI v2 magic")
  } else {
    problem("nibabel reports magic '%s', expected 6e2b32000d0a1a0a", str_value(dump, "raw_magic"))
  }
  if (identical(str_value(dump, "raw_extension_flag"), "01000000")) {
    report_ok("nibabel reports the extension flag bytes")
  } else {
    problem("nibabel reports extension flag '%s', expected 01000000", str_value(dump, "raw_extension_flag"))
  }

  # The extensions, as nibabel reads them.
  if (length(dump_ext) == 2L && dump_ext[[1L]]$ecode == 6L && dump_ext[[2L]]$ecode == 4L) {
    report_ok("nibabel reads both extension codes")
  } else {
    problem("nibabel reads %d extensions, expected 2 with the codes 6 and 4", length(dump_ext))
  }
  if (length(dump_ext) == 2L &&
      dump_ext[[1L]]$size == nifti2.extension.size(ext_text) &&
      dump_ext[[2L]]$size == nifti2.extension.size(ext_raw)) {
    report_ok("nibabel reports the extension sizes on disk")
  } else {
    problem("nibabel reports the sizes %s, expected %s",
            paste(vapply(dump_ext, function(e) e$size, integer(1L)), collapse = ","),
            paste(nifti2.extension.size(ext_text), nifti2.extension.size(ext_raw), collapse = ","))
  }
  if (length(dump_ext) == 2L && all(vapply(dump_ext, function(e) e$size %% 16L, integer(1L)) == 0L)) {
    report_ok("the extension sizes are multiples of 16 bytes")
  } else {
    problem("an extension size is not a multiple of 16 bytes")
  }
  # The payloads, compared byte by byte with what we wrote (the trailing NUL padding is not part of them).
  payload_text <- readBin(file.path(payload_dir, "0.bin"), "raw", n = 1000L)
  payload_raw <- readBin(file.path(payload_dir, "1.bin"), "raw", n = 1000L)
  if (identical(payload_text, ext_text$content)) {
    report_ok("nibabel reads the text payload exactly")
  } else {
    problem("the text payload differs: '%s'", rawToChar(payload_text))
  }
  if (identical(payload_raw, ext_raw$content)) {
    report_ok("nibabel reads the binary payload exactly")
  } else {
    problem("the binary payload differs")
  }

  # The data offset: the extensions end at 544 + their sizes, and the data starts there.
  expected_vox_offset <- 544 + nifti2.extension.size(ext_text) + nifti2.extension.size(ext_raw)
  if (num_value(dump, "raw_vox_offset") == expected_vox_offset) {
    report_ok("the data offset is behind the extensions")
  } else {
    problem("the data offset is %g, expected %g", num_value(dump, "raw_vox_offset"), expected_vox_offset)
  }
  if (identical(str_value(dump, "dim"), "3 4 3 2 1 1 1 1")) {
    report_ok("nibabel reports the data dimensions")
  } else {
    problem("nibabel reports dim '%s'", str_value(dump, "dim"))
  }
  if (identical(str_value(dump, "dtype"), "float32") && num_value(dump, "num_values") == 24 &&
      abs(num_value(dump, "sum") - sum(niidata)) < 0.001) {
    report_ok("nibabel reads the voxel data behind the extensions")
  } else {
    problem("nibabel reads %g values with sum %g", num_value(dump, "num_values"), num_value(dump, "sum"))
  }
  if (identical(str_value(dump, "raw_pixdim"), "1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0")) {
    report_ok("the voxel sizes of the written header are positive")
  } else {
    problem("the written pixdim is '%s'", str_value(dump, "raw_pixdim"))
  }
  # We can read back what nibabel saw, i.e. our own reader agrees with the file.
  hdr_read <- read.nifti2.header(our_file)
  if (length(hdr_read$extensions) == 2L && hdr_read$vox_offset == expected_vox_offset) {
    report_ok("our reader agrees with the file layout")
  } else {
    problem("our reader reports %d extensions and vox_offset %d", length(hdr_read$extensions), hdr_read$vox_offset)
  }

  # A file without extensions: nibabel must see the four zero flag bytes and the data at 544.
  our_file_no_ext <- file.path(work_dir, "ours_no_ext.nii")
  write.nifti2(our_file_no_ext, niidata)
  lines_no_ext <- dump_file(our_file_no_ext)
  dump_no_ext <- read_named_values(lines_no_ext)
  if (length(read_dump_extensions(lines_no_ext)) == 0L && identical(str_value(dump_no_ext, "raw_extension_flag"), "00000000") &&
      num_value(dump_no_ext, "raw_vox_offset") == 544) {
    report_ok("a file without extensions is unchanged")
  } else {
    problem("the file without extensions has flag '%s', offset %g", str_value(dump_no_ext, "raw_extension_flag"),
            num_value(dump_no_ext, "raw_vox_offset"))
  }

  # A gzipped file with extensions.
  our_file_gz <- file.path(work_dir, "ours_with_ext.nii.gz")
  write.nifti2(our_file_gz, niidata, extensions = list(ext_text))
  lines_gz <- dump_file(our_file_gz)
  dump_gz <- read_named_values(lines_gz)
  ext_gz <- read_dump_extensions(lines_gz)
  if (length(ext_gz) == 1L && ext_gz[[1L]]$ecode == 6L && num_value(dump_gz, "num_values") == 24) {
    report_ok("nibabel reads a compressed file with extensions")
  } else {
    problem("nibabel reads %d extensions in the compressed file", length(ext_gz))
  }

  # =====================================================================================
  cat("\nPart 2: we read a NIFTI v2 file written by nibabel.\n")
  # =====================================================================================

  reference_file <- file.path(work_dir, "reference.nii")
  reference_gz <- file.path(work_dir, "reference.nii.gz")
  writer <- file.path("dev_tools", "nibabel_nifti2_dump.py")
  write_out <- suppressWarnings(run_python(c(writer, "write_reference", reference_file, reference_gz)))
  if (!file.exists(reference_file)) {
    report_skip("all part 2 checks", paste("could not create the reference file:", paste(write_out, collapse = " ")))
  } else {
    payload_dir_ref <- file.path(work_dir, "payloads_ref")
    lines_ref <- dump_file(reference_file, payload_dir_ref)
    dump_ref <- read_named_values(lines_ref)
    ext_ref <- read_dump_extensions(lines_ref)

    hdr_ref <- read.nifti2.header(reference_file)
    if (length(hdr_ref$extensions) == length(ext_ref) && length(ext_ref) == 2L) {
      report_ok("we read the number of extensions nibabel wrote")
    } else {
      problem("we read %d extensions, nibabel reports %d", length(hdr_ref$extensions), length(ext_ref))
    }
    if (length(hdr_ref$extensions) == 2L && hdr_ref$extensions[[1L]]$ecode == 6L && hdr_ref$extensions[[2L]]$ecode == 4L) {
      report_ok("we read the extension codes nibabel wrote")
    } else {
      problem("the extension codes differ")
    }
    if (length(hdr_ref$extensions) == 2L &&
        identical(hdr_ref$extensions[[1L]]$content, readBin(file.path(payload_dir_ref, "0.bin"), "raw", n = 1000L)) &&
        identical(hdr_ref$extensions[[2L]]$content, readBin(file.path(payload_dir_ref, "1.bin"), "raw", n = 1000L))) {
      report_ok("we read the payloads byte for byte")
    } else {
      problem("the payloads differ from what nibabel reports")
    }
    if (hdr_ref$vox_offset == num_value(dump_ref, "raw_vox_offset")) {
      report_ok("we agree with nibabel on the data offset")
    } else {
      problem("our data offset is %d, nibabel reports %g", hdr_ref$vox_offset, num_value(dump_ref, "raw_vox_offset"))
    }
    data_ref <- read.nifti2.data(reference_file, header = hdr_ref)
    if (abs(sum(as.double(data_ref)) - num_value(dump_ref, "sum")) < 0.001) {
      report_ok("we read the voxel data of the nibabel file")
    } else {
      problem("the data sum is %f, nibabel reports %g", sum(as.double(data_ref)), num_value(dump_ref, "sum"))
    }
    # The same file, gzipped: our reader handles the extensions of compressed files as well.
    hdr_ref_gz <- read.nifti2.header(reference_gz)
    if (length(hdr_ref_gz$extensions) == 2L &&
        identical(hdr_ref_gz$extensions[[1L]]$content, hdr_ref$extensions[[1L]]$content)) {
      report_ok("we read the extensions of the compressed file")
    } else {
      problem("the extensions of the compressed file differ")
    }
  }

  # =====================================================================================
  cat("\nPart 3: the CIFTI-2 example files written by Connectome Workbench.\n")
  # =====================================================================================

  cifti_files <- c(file.path("extra_test_data", "cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"),
                   file.path("extra_test_data", "cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii"),
                   file.path("extra_test_data", "cifti", "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii"))
  num_checked_files <- 0L
  for (cifti_file in cifti_files) {
    if (!file.exists(cifti_file)) {
      next
    }
    num_checked_files <- num_checked_files + 1L
    payload_dir_cifti <- file.path(work_dir, paste0("payloads_cifti_", num_checked_files))
    lines_cifti <- dump_file(cifti_file, payload_dir_cifti)
    ext_cifti <- read_dump_extensions(lines_cifti)
    hdr_cifti <- read.nifti2.header(cifti_file)
    cifti_ext <- nifti2.get.extension(hdr_cifti, 32L)
    label <- sprintf("the XML metadata of %s", basename(cifti_file))
    if (length(ext_cifti) == 1L && ext_cifti[[1L]]$ecode == 32L && !is.null(cifti_ext) &&
        identical(cifti_ext$content, readBin(file.path(payload_dir_cifti, "0.bin"), "raw", n = 1e7))) {
      report_ok(label)
    } else {
      problem("%s differs from what nibabel reports", label)
    }
    # The XML payload must be parseable, i.e. the NUL padding was removed correctly.
    xml_label <- sprintf("the XML of %s parses", basename(cifti_file))
    xml_ok <- !inherits(try(xml2::read_xml(nifti2.extension.content(cifti_ext, strip_nul = FALSE)), silent = TRUE), "try-error")
    if (xml_ok) {
      report_ok(xml_label)
    } else {
      problem("%s is not valid XML", xml_label)
    }
  }
  if (num_checked_files == 0L) {
    report_skip("the CIFTI-2 fixture checks", "the CIFTI-2 example files are not available.")
  }

  # Connectome Workbench is the reference implementation of the format: it must accept a NIFTI v2 file that we
  # wrote (this is exactly the check that the old NIFTI v1 magic string failed, with 'incorrect magic').
  wb_command <- Sys.getenv("WB_COMMAND", unset = "~/software/connectome_workbench/workbench/bin_linux64/wb_command")
  if (file.exists(path.expand(wb_command))) {
    wb_out <- suppressWarnings(system2(path.expand(wb_command),
                                       c("-nifti-information", our_file, "-print-header"),
                                       stdout = TRUE, stderr = TRUE))
    wb_status <- attr(wb_out, "status")
    wb_ok <- (is.null(wb_status) || wb_status == 0L) && any(grepl("magic: n+2", wb_out, fixed = TRUE)) &&
      any(grepl("sizeof_hdr: 540", wb_out, fixed = TRUE))
    if (wb_ok) {
      report_ok("Connectome Workbench reads our NIFTI v2 file")
    } else {
      problem("Connectome Workbench rejected our file: %s", paste(utils::head(wb_out, 3L), collapse = " "))
    }
    # Workbench reports the data offset it computed, which must be the one we wrote.
    vox_line <- wb_out[grepl("vox_offset:", wb_out, fixed = TRUE)]
    wb_vox_offset <- if (length(vox_line) > 0L) as.integer(sub(".*vox_offset:[[:space:]]*", "", vox_line[1L])) else NA_integer_
    if (!is.na(wb_vox_offset) && wb_vox_offset == expected_vox_offset) {
      report_ok("Workbench agrees with us on the data offset")
    } else {
      problem("Workbench reports vox_offset %s, expected %d", as.character(wb_vox_offset), expected_vox_offset)
    }
  } else {
    report_skip("the Connectome Workbench check", "wb_command not found. Set the WB_COMMAND environment variable.")
  }
}

cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", num_checks, num_failures, num_skipped))

if (num_failures == 0L) {
  cat("All NIFTI v2 extension checks passed.\n")
} else {
  stop(sprintf("%d NIFTI v2 extension checks failed.", num_failures))
}
