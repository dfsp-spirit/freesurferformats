# Check the CIFTI-2 reader against the two reference implementations:
#
#  1. Connectome Workbench (the reference *implementation* of the format): its
#     '-cifti-convert -to-text' output is a text version of the data matrix, which is
#     compared with the values our reader returns (the text file has one line per index
#     of matrix dimension 1, and one value per index of matrix dimension 0, i.e. it is
#     the transpose of what read.cifti() returns).
#  2. nibabel: dev_tools/cifti_dump.py prints the parsed metadata and the data values,
#     dev_tools/cifti_dump.R prints the same from our reader, and the two outputs are
#     compared with a text diff (see dev_tools/README or the plan for the details).
#
# Requires: the 'wb_command' binary (Connectome Workbench) and a Python with nibabel
# (default: the venv at ~/develop/brain_atlases/.venv, which is where nibabel was
# installed for this project). Usage:
#
#   Rscript dev_tools/check_cifti_conversion.R [FILE ...]
#
# Without arguments, the shipped fixtures and the official example files are used.

suppressMessages(devtools::load_all("/home/ts/develop/freesurferformats", quiet = TRUE))

wb_command <- Sys.getenv("WB_COMMAND", unset = path.expand("~/software/connectome_workbench/workbench/bin_linux64/wb_command"))
python <- Sys.getenv("CIFTI_CHECK_PYTHON", unset = path.expand("~/develop/brain_atlases/.venv/bin/python"))
repo_dir <- "/home/ts/develop/freesurferformats"
tmp_dir <- file.path(tempdir(), "cifti_check")
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

# --- 1. Workbench: compare the values ----------------------------------------

workbench_check <- function(filepath) {
  text_file <- file.path(tmp_dir, paste0(basename(filepath), ".txt"))
  status <- system2(wb_command, c("-cifti-convert", "-to-text", shQuote(filepath), shQuote(text_file)),
                    stdout = FALSE, stderr = FALSE)
  if (status != 0) {
    return(sprintf("FAIL (wb_command -cifti-convert -to-text failed with status %d)", status))
  }
  wb_values <- as.matrix(utils::read.table(text_file, header = FALSE))
  our_values <- t(read.cifti(filepath)$data)
  if (!identical(dim(wb_values), dim(our_values))) {
    return(sprintf("FAIL (dimensions differ: Workbench %s, ours %s)",
                   paste(dim(wb_values), collapse = "x"), paste(dim(our_values), collapse = "x")))
  }
  # Workbench's text output has 6 significant digits, so compare with a tolerance that
  # accounts for that (and report the largest relative difference).
  rel_diff <- abs(wb_values - our_values) / pmax(abs(our_values), 1e-6)
  max_rel_diff <- max(rel_diff)
  if (max_rel_diff > 1e-5) {
    return(sprintf("FAIL (values differ, max relative difference %.8f)", max_rel_diff))
  }
  return(sprintf("PASS (%d x %d values, max relative difference %.2e)", nrow(our_values), ncol(our_values), max_rel_diff))
}

# --- 2. nibabel: compare the parsed dump -------------------------------------

dump_check <- function(files) {
  py_dump <- file.path(tmp_dir, "dump_python.txt")
  r_dump <- file.path(tmp_dir, "dump_r.txt")
  py_status <- system2(python, c(file.path(repo_dir, "dev_tools", "cifti_dump.py"), shQuote(files)),
                       stdout = py_dump, stderr = FALSE)
  if (py_status != 0) {
    return("FAIL (dev_tools/cifti_dump.py failed, is nibabel installed in the Python interpreter?)")
  }
  system2("Rscript", c(file.path(repo_dir, "dev_tools", "cifti_dump.R"), shQuote(files)),
          stdout = r_dump, stderr = FALSE)
  diff_output <- suppressWarnings(system2("diff", c(shQuote(py_dump), shQuote(r_dump)), stdout = TRUE))
  if (length(diff_output) > 0L) {
    return(sprintf("FAIL (%d differing lines, see %s)", length(diff_output), py_dump))
  }
  return(sprintf("PASS (%d identical dump lines)", length(readLines(py_dump))))
}

# --- 3. the writers for connectomes and parcellated files --------------------

# Write a file from the axes and the data of an existing CIFTI-2 file, using the
# user-facing writer that fits its type. Returns the path of the file written.
write_like <- function(fixture) {
  data <- read.cifti(fixture)$data
  out_file <- file.path(tmp_dir, paste0("written_", basename(fixture)))
  base_name <- basename(fixture)
  if (grepl("(dconn|pconn|pdconn|dpconn)[.]nii$", base_name)) {
    write.fs.connectome.cifti(out_file, data, template = fixture)
  } else if (grepl("(pscalar|ptseries)[.]nii$", base_name)) {
    write.fs.parcellated.cifti(out_file, data, template = fixture)
  } else {
    write.cifti(out_file, data, template = fixture)
  }
  return(out_file)
}

# The decisive check for the parcel handling: the official ptseries of the Conte69 example
# data is the mean of the official dtseries over the vertices of each of its parcels, so
# reproducing those values from the parcels axis of the file proves that the vertex lists,
# the parcel order and the data layout of our writer are right. The axis comes from the
# official file, the numbers are computed here and written with our writer.
parcels_semantic_check <- function() {
  dtseries <- file.path(repo_dir, "extra_test_data", "cifti",
                        "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii")
  ptseries <- file.path(repo_dir, "extra_test_data", "cifti",
                        "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii")
  if (!file.exists(dtseries) || !file.exists(ptseries)) {
    return("SKIPPED (the official Conte69 example files are missing)")
  }
  axis <- cifti.axis.from.template(ptseries, dim = 1L)
  series_data <- read.cifti(dtseries)$data
  # The vertex indices of a parcel are per brain structure (and the left/right indices
  # overlap numerically), so the columns of a parcel have to be looked up in the
  # brainordinate table of the file, not by the index alone.
  grayordinates <- cifti.grayordinates(read.cifti.header(dtseries), 1L)
  # One column per parcel, one row per series point (matrix dimension 0 first).
  means <- vapply(axis$parcels, function(parcel) {
    columns <- integer(0L)
    for (structure_name in names(parcel$vertices)) {
      columns <- c(columns, which(grayordinates$structure_short == cifti.structure.short(structure_name) &
                                    grayordinates$vertex_index %in% parcel$vertices[[structure_name]]))
    }
    return(rowMeans(series_data[, columns, drop = FALSE]))
  }, numeric(nrow(series_data)))
  expected <- read.cifti(ptseries)$data
  our_file <- file.path(tmp_dir, "semantic.ptseries.nii")
  write.fs.parcellated.cifti(our_file, means, template = ptseries)
  our_values <- read.cifti(our_file)$data

  max_abs_diff <- max(abs(our_values - expected))
  if (max_abs_diff > 1e-5) {
    return(sprintf("FAIL (the official values are not reproduced, max absolute difference %.8f)", max_abs_diff))
  }
  wb_result <- workbench_check(our_file)
  if (startsWith(wb_result, "FAIL")) {
    return(sprintf("FAIL (Workbench does not agree, %s)", wb_result))
  }
  return(sprintf("PASS (%d parcels, %s values, official values reproduced, max absolute difference %.2e)",
                 length(axis$parcels), paste(dim(expected), collapse = "x"), max_abs_diff))
}

# Building a parcels axis from real FreeSurfer annotations and writing a parcellated file
# with it: the vertex lists have to cover the surface exactly and the region names and
# their order have to survive the round trip.
annot_parcels_check <- function() {
  annot_file <- file.path(repo_dir, "inst", "extdata", "lh.aparc.annot.gz")
  annot <- read.fs.annot(annot_file)
  axis <- cifti.axis.parcels.from.annot(list(lh = annot, rh = annot))
  out_file <- file.path(tmp_dir, "annot_parcels.pscalar.nii")
  values <- as.numeric(seq_along(axis$parcels))
  write.fs.parcellated.cifti(out_file, values, axes = axis, map_names = "region_size")

  back <- read.cifti(out_file)
  back_names <- as.character(cifti.parcels(back$header, 1L)$name)
  if (!identical(back_names, vapply(axis$parcels, function(parcel) parcel$name, character(1L)))) {
    return("FAIL (the parcel names do not survive the round trip)")
  }
  if (!identical(as.numeric(back$data[1L, ]), values)) {
    return("FAIL (the values do not survive the round trip)")
  }
  covered <- sort(unlist(lapply(axis$parcels, function(parcel) parcel$vertices$CORTEX_LEFT)))
  if (!identical(covered, annot$vertices)) {
    return("FAIL (the parcels do not cover the annotation exactly)")
  }
  wb_result <- workbench_check(out_file)
  if (startsWith(wb_result, "FAIL")) {
    return(sprintf("FAIL (Workbench does not agree, %s)", wb_result))
  }
  return(sprintf("PASS (%d parcels from 2 annotations with %d vertices each, checked by Workbench)",
                 length(axis$parcels), length(annot$vertices)))
}

# --- run the checks ----------------------------------------------------------

files <- commandArgs(trailingOnly = TRUE)
if (length(files) == 0L) {
  files <- c(
    list.files(file.path(repo_dir, "inst", "extdata", "cifti"), pattern = "[.]nii$", full.names = TRUE),
    list.files(file.path(repo_dir, "extra_test_data", "cifti"), pattern = "[.]nii$", full.names = TRUE)
  )
}

failures <- 0L
if (file.exists(wb_command)) {
  for (filepath in files) {
    result <- workbench_check(filepath)
    if (startsWith(result, "FAIL")) {
      failures <- failures + 1L
    }
    cat(sprintf("Workbench  %-34s %s\n", basename(filepath), result))
  }
} else {
  cat(sprintf("SKIPPED: Workbench not found at '%s' (set WB_COMMAND).\n", wb_command))
  failures <- failures + 1L
}

# The mixed file cannot be loaded by nibabel (see dev_tools/cifti_dump.py), it is checked
# by Workbench only.
dump_result <- dump_check(files)
if (startsWith(dump_result, "FAIL")) {
  failures <- failures + 1L
}
cat(sprintf("nibabel    %-34s %s\n", paste(length(files), "files"), dump_result))

# --- the files our writers produce -------------------------------------------
cat("\n-- files written with the user-facing CIFTI-2 writers --\n")

writer_fixtures <- files[grepl("(dconn|pconn|pdconn|dpconn|pscalar|ptseries)[.]nii$", basename(files))]
written <- character(0L)
for (fixture in writer_fixtures) {
  out_file <- write_like(fixture)
  written <- c(written, out_file)
  result <- workbench_check(out_file)
  if (startsWith(result, "FAIL")) {
    failures <- failures + 1L
  }
  cat(sprintf("Workbench  %-34s %s\n", basename(out_file), result))
}
if (length(written) > 0L) {
  dump_result <- dump_check(written)
  if (startsWith(dump_result, "FAIL")) {
    failures <- failures + 1L
  }
  cat(sprintf("nibabel    %-34s %s\n", paste(length(written), "written files"), dump_result))
}

special_checks <- list(parcels_semantic_check = parcels_semantic_check,
                       annot_parcels_check = annot_parcels_check)
for (check_name in names(special_checks)) {
  result <- special_checks[[check_name]]()
  if (startsWith(result, "FAIL") || startsWith(result, "SKIPPED")) {
    failures <- failures + 1L
  }
  cat(sprintf("%-10s %-34s %s\n", "writers", check_name, result))
}

cat(sprintf("\n%s (%d failures)\n", if (failures == 0L) "ALL CHECKS PASSED" else "CHECKS FAILED", failures))
quit(status = if (failures == 0L) 0L else 1L)
