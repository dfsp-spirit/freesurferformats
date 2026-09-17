# Manual validation script for the TCK/TSF track file readers against very large
# real-world tractograms. Not part of the automated test suite, since it needs
# files in the multi-GB range that are not shipped with the package.
#
# Usage:
#   Rscript dev_tools/check_large_tck.R <path/to/file.tck[.gz]> [n_tracks]
#
# The script checks the following, without ever loading the whole file:
#   1. The header is read correctly.
#   2. Reading a subset via max_tracks works and is fast.
#   3. For a gzip-compressed file, the coordinates of the first N tracks are
#      identical to those read from the uncompressed file, which validates the
#      transparent gzip handling on real data.
#   4. Requesting the whole payload of a file that cannot fit into memory fails
#      with a helpful message instead of an obscure allocation error.

suppressMessages(devtools::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript dev_tools/check_large_tck.R <path/to/file.tck[.gz]> [n_tracks]");
}
filepath <- args[1];
n_tracks <- if (length(args) >= 2L) as.integer(args[2]) else 10L;

if (!file.exists(filepath)) {
  stop(sprintf("File '%s' does not exist.\n", filepath));
}

size_gb <- file.size(filepath) / 1e9;
cat(sprintf("File: %s (%.2f GB)\n", filepath, size_gb));

## 1. Header only.
cat("\n-- header --\n");
hdr <- read.mrtrix.header(filepath);   # internal helper, available via load_all()
cat(sprintf("  gzipped: %s\n", hdr$gzipped));
cat(sprintf("  header lines: %d\n", length(hdr$lines)));
cat(sprintf("  count (as stored): %s\n", hdr$header$count));
cat(sprintf("  datatype: %s\n", hdr$header$datatype));
cat(sprintf("  data offset: %s bytes\n", hdr$header$file));
cat("  keys:", paste(names(hdr$header), collapse = ", "), "\n");

## 2. Subset read.
cat(sprintf("\n-- reading the first %d tracks --\n", n_tracks));
invisible(gc(reset = TRUE));
tm <- system.time(tracks <- freesurferformats::read.dti.tck(filepath, max_tracks = n_tracks))["elapsed"];
cat(sprintf("  elapsed: %.2f s\n", tm));
cat(sprintf("  tracks: %d\n", length(tracks$tracks)));
cat(sprintf("  points: %d\n", nrow(freesurferformats::fs.tracts.coords(tracks$tracks))));
mem <- invisible(gc());   # 2 rows (Ncells, Vcells), column 6 is 'max used' in MB
cat(sprintf("  peak R memory: %.1f MB\n", max(mem[, 6])));
cat(sprintf("  coord range: %.2f .. %.2f\n",
            min(freesurferformats::fs.tracts.coords(tracks$tracks)),
            max(freesurferformats::fs.tracts.coords(tracks$tracks))));
cat(sprintf("  points per track: %s\n",
            paste(freesurferformats::fs.tracts.lengths(tracks$tracks)[1:min(10L, n_tracks)], collapse = ", ")));

## 3. gz vs uncompressed.
plain_path <- sub("\\.gz$", "", filepath);
if (endsWith(filepath, ".gz") && file.exists(plain_path)) {
  cat("\n-- gz vs uncompressed (first N tracks) --\n");
  plain <- freesurferformats::read.dti.tck(plain_path, max_tracks = n_tracks);
  same <- isTRUE(all.equal(freesurferformats::fs.tracts.coords(tracks$tracks),
                           freesurferformats::fs.tracts.coords(plain$tracks)));
  cat(sprintf("  coordinates identical: %s\n", same));
  if (!same) {
    cat("  gz  :", freesurferformats::fs.tracts.coords(tracks$tracks)[1, ], "\n");
    cat("  plain:", freesurferformats::fs.tracts.coords(plain$tracks)[1, ], "\n");
  }
  cat(sprintf("  tracks equal: %s\n",
              identical(freesurferformats::fs.tracts.lengths(tracks$tracks),
                        freesurferformats::fs.tracts.lengths(plain$tracks))));
}

## 4. Whole-file read must fail with a clear message for files that are too big.
cat("\n-- whole file read (expected to hit the safety limit) --\n");
res <- tryCatch(freesurferformats::read.dti.tck(filepath), error = function(e) conditionMessage(e));
if (is.character(res)) {
  cat("  error message:\n");
  cat(paste0("    ", strsplit(res, "\n")[[1]], collapse = "\n"), "\n");
} else {
  cat(sprintf("  no error, read %d tracks (file fits into memory here)\n", length(res$tracks)));
}
