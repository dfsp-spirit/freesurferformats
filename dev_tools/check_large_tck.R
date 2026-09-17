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
#   5. The header-only reader, the streaming iterator and the bounding box filter
#      work on the real file.
#   6. Reading a subset and writing it back to a new file round-trips exactly
#      (this is the 'crop a huge tractogram' workflow).
#
# Usage:
#   Rscript dev_tools/check_large_tck.R <path/to/file.tck[.gz]> [n_tracks] [full_scan]
#
# The optional third argument 'full_scan' (TRUE/FALSE, default FALSE) enables the
# whole-file track count, which scans the complete payload and can take minutes
# on a multi-GB file.

suppressMessages(devtools::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE);
if (length(args) < 1L) {
  stop("Usage: Rscript dev_tools/check_large_tck.R <path/to/file.tck[.gz]> [n_tracks] [full_scan]");
}
filepath <- args[1];
n_tracks <- if (length(args) >= 2L) as.integer(args[2]) else 10L;
full_scan <- if (length(args) >= 3L) isTRUE(as.logical(args[3])) else FALSE;

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

## 5. Header-only reader, iterator, bounding box filter.
cat("\n-- header-only reader --\n");
invisible(gc(reset = TRUE));
tm <- system.time(hdr_only <- freesurferformats::read.dti.tck.header(filepath))["elapsed"];
cat(sprintf("  elapsed: %.3f s\n", tm));
cat(sprintf("  count: %s, datatype: %s\n", hdr_only$count, hdr_only$datatype));
cat(sprintf("  peak R memory: %.1f MB\n", max(invisible(gc())[, 6])));

cat("\n-- streaming iterator --\n");
invisible(gc(reset = TRUE));
tm <- system.time({
  it <- freesurferformats::dti.track.iterator(filepath);
  iter_tracks <- 0L; iter_points <- 0L; first_iter <- NULL;
  while (iter_tracks < n_tracks) {
    tr <- it$next.track();
    if (is.null(tr)) break;
    iter_tracks <- iter_tracks + 1L;
    iter_points <- iter_points + nrow(tr);
    if (is.null(first_iter)) first_iter <- tr;
  }
  # Live memory after a collection, as opposed to the peak, which also includes
  # garbage that R has not collected yet. This must not grow with the number of
  # tracks that were consumed.
  iter_live_mb <- sum(gc()[, 2]);
  it$close();
})["elapsed"];
cat(sprintf("  elapsed: %.2f s for %d tracks (%d points)\n", tm, iter_tracks, iter_points));
cat(sprintf("  memory held by the iterator: %.1f MB (constant, one chunk is buffered)\n", iter_live_mb));
cat(sprintf("  first track from the iterator has %d points, first point: %s\n",
            nrow(first_iter), paste(round(first_iter[1, ], 2), collapse = ", ")));

cat("\n-- bounding box filter --\n");
bb <- coord.bbox(freesurferformats::fs.tracts.coords(tracks$tracks));   # internal helper
cat(sprintf("  box taken from the first %d tracks: %s\n", n_tracks, paste(round(bb, 2), collapse = ", ")));
tm <- system.time(filtered <- freesurferformats::read.dti.tck(filepath, bbox = bb, max_tracks = n_tracks))["elapsed"];
fcoords <- freesurferformats::fs.tracts.coords(filtered$tracks);
flens <- freesurferformats::fs.tracts.lengths(filtered$tracks);
cat(sprintf("  elapsed: %.2f s, selected %d tracks (%d points)\n", tm, length(filtered$tracks), nrow(fcoords)));
# Every selected track must have at least one point inside the box (the tracks may
# well have other points outside of it).
all_inside <- TRUE; pos <- 1L;
for (L in flens) {
  seg <- fcoords[pos:(pos + L - 1L), , drop = FALSE]; pos <- pos + L;
  if (!any(seg[, 1L] >= bb[1L] & seg[, 1L] <= bb[2L] & seg[, 2L] >= bb[3L] & seg[, 2L] <= bb[4L] &
           seg[, 3L] >= bb[5L] & seg[, 3L] <= bb[6L])) {
    all_inside <- FALSE; break;
  }
}
cat(sprintf("  every selected track has a point inside the box: %s\n", all_inside));
cat(sprintf("  all %d input tracks were selected: %s\n", n_tracks, length(filtered$tracks) == n_tracks));

if (full_scan) {
  cat("\n-- full track count scan (this reads the whole payload) --\n");
  tm <- system.time(cnt <- freesurferformats::dti.track.count(filepath))["elapsed"];
  cat(sprintf("  elapsed: %.1f s\n", tm));
  cat(sprintf("  counted tracks: %s (header says: %s)\n", cnt, hdr_only$count));
  cat(sprintf("  count matches header: %s\n", isTRUE(as.numeric(cnt) == as.numeric(hdr_only$count))));
  tm <- system.time(fbb <- freesurferformats::dti.track.bbox(filepath))["elapsed"];
  cat(sprintf("  bbox scan elapsed: %.1f s, bbox: %s\n", tm, paste(round(fbb, 2), collapse = ", ")));
} else {
  cat("\n-- full track count scan skipped (pass 'TRUE' as 3rd argument to enable it) --\n");
}

## 6. Crop and write round trip.
cat(sprintf("\n-- crop workflow: read %d tracks, write, read back --\n", n_tracks));
outfile <- tempfile(fileext = if (endsWith(filepath, ".gz")) ".tck.gz" else ".tck");
invisible(gc(reset = TRUE));
tm <- system.time({
  sub <- freesurferformats::read.dti.tck(filepath, max_tracks = n_tracks);
  freesurferformats::write.dti.tck(sub$tracks, outfile, header = sub$header);
  back <- freesurferformats::read.dti.tck(outfile);
})["elapsed"];
cat(sprintf("  elapsed: %.2f s, output file: %.1f MB\n", tm, file.size(outfile) / 1e6));
cat(sprintf("  tracks: %d -> %d\n", length(sub$tracks), length(back$tracks)));
cat(sprintf("  coordinates round-tripped exactly: %s\n",
            isTRUE(all.equal(freesurferformats::fs.tracts.coords(sub$tracks),
                             freesurferformats::fs.tracts.coords(back$tracks)))));
cat(sprintf("  track lengths round-tripped exactly: %s\n",
            identical(freesurferformats::fs.tracts.lengths(sub$tracks),
                      freesurferformats::fs.tracts.lengths(back$tracks))));
cat(sprintf("  header keys kept: %d of %d\n",
            length(intersect(names(sub$header), names(back$header))), length(sub$header)));
cat(sprintf("  written file is gzipped: %s\n", is.gzip.file(outfile)));
unlink(outfile);
