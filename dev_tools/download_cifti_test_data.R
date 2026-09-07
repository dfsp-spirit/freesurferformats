#!/usr/bin/env Rscript
#
# One-time helper to fetch the official CIFTI-2 example files and copy the ones that are
# needed for the unit tests into the repository's `extra_test_data/cifti/` directory.
#
# The example files are provided under the Open Data Commons Public Domain Dedication and
# Licence (PDDL), see http://opendatacommons.org/licenses/pddl/1.0/ for details. They are
# downloaded from https://www.nitrc.org/projects/cifti/ via the `cifti` package's
# `download_cifti_data()` function.
#
# Usage (from the repository root):
#   Rscript dev_tools/download_cifti_test_data.R
#
# This downloads ~27 MB and copies only the files needed by the unit tests (see
# tests/testthat/test-cifti2.R) into the repository. `extra_test_data/` is excluded from
# the built/installed package via `.Rbuildignore`, so these files do not count against the
# 5 MB CRAN package size limit.

# Files needed by the unit tests, relative to the cifti example data directory.
needed_files <- c(
  "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii",
  "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii",
  "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii"
)

if (!requireNamespace("cifti", quietly = TRUE)) {
  stop("This script requires the 'cifti' package to be installed.")
}

# Download (and extract) the official CIFTI-2 example set. Returns the paths of the
# individual extracted files.
example_files <- cifti::download_cifti_data()

# All example files live in a single directory; derive it from the returned paths.
example_dir <- unique(dirname(example_files))
if (length(example_dir) != 1L) {
  stop(sprintf("Expected all CIFTI example files to live in a single directory, but found %d: %s",
               length(example_dir), paste(example_dir, collapse = ", ")))
}

target_dir <- file.path("extra_test_data", "cifti")
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

for (fname in needed_files) {
  src <- file.path(example_dir, fname)
  dst <- file.path(target_dir, fname)
  if (!file.exists(src)) {
    stop(sprintf("Expected CIFTI example file not found after download: %s", src))
  }
  file.copy(src, dst, overwrite = TRUE)
  cat("Copied", fname, "->", dst, "\n")
}

cat("Done. The files in", target_dir, "are now ready to be committed to the repository.\n")
