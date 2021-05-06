#!/usr/bin/env Rscript
#
# annot_unique_across_hemis.R -- Rewrite an annotation so that the region IDs and names are unique across hemispheres.
# This is an example script that comes with 'freesurferformats': https://github.com/dfsp-spirit/freesurferformats
#
# Dependencies:
#   Requires the 'freesurferformats' package to be installed.
#
#   NOTE: Currently, you need the latest development version of 'fsbrain' for this to work. To install it, run in your R session:
#
#            install.packages(c("devtools", "knitr", "rmarkdown", "testthat"));
#            devtools::install_github("dfsp-spirit/freesurferformats");
#
# USAGE: ./annot_unique_across_hemis.R <lh_annot_in> <rh_annot_in> <lh_annot_out> <rh_annot_out>
#
# Written by Tim Schaefer


library("freesurferformats");
args = commandArgs(trailingOnly=TRUE);


annot_unique <- function(lh_annot_in, rh_annot_in, lh_annot_out, rh_annot_out) {
  lh_annot = freesurferformats::read.fs.annot(lh_annot_in);
  rh_annot = freesurferformats::read.fs.annot(rh_annot_in);

  # We leave the lh annot as-is and only modify the right hemi annot. The next line is not strictly required.
  lh_annot_unique = freesurferformats::annot.unique(lh_annot, add_to_region_indices = 0L, region_name_prefix=NULL, region_name_suffix=NULL);
  #lh_annot_unique = lh_annot;

  # The important part: make rh unique.
  rh_annot_unique = annot.unique(rh_annot, freesurferformats::annot.max.region.idx(lh_annot)+1L, region_name_prefix='rh_');

  # Save results as files.
  freesurferformats::write.fs.annot(lh_annot_out, fs.annot = lh_annot_unique);
  cat(sprintf("Annot for left hemi written to '%s'.\n", lh_annot_out));

  freesurferformats::write.fs.annot(rh_annot_out, fs.annot = rh_annot_unique);
  cat(sprintf("Annot for right hemi written to '%s'.\n", rh_annot_out));
}


if (length(args) == 4) {
  annot_unique(args[1], args[2], args[3], args[4]);
} else {
  stop("USAGE: ./annot_unique_across_hemis.R <lh_annot_in> <rh_annot_in> <lh_annot_out> <rh_annot_out>");
}
