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

cat(sprintf("\n%s (%d failures)\n", if (failures == 0L) "ALL CHECKS PASSED" else "CHECKS FAILED", failures))
quit(status = if (failures == 0L) 0L else 1L)
