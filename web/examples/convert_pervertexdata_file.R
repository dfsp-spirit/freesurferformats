#!/usr/bin/env Rscript
# Convert a file with per-vertex data to a new format.

library("freesurferformats");

usage_msg <- function() { return(sprintf("USAGE: convert_pervertexdata_file <infile> <outfile>\n")); }

if (length(args) != 2) {
    stop(usage_msg());
}

infile = args[1];
outfile = args[2];

if(! file.exists(infile)) { stop(sprintf("Infile %s cannot be read.", infile)); }

cat(sprintf("Converting input per-vertex data file '%s' to target file '%s'...\n", infile, outfile));
pvd = freesurferformats::read.fs.morph(infile);
freesurferformats::write.fs.morph(outfile, pvd);
cat(sprintf("Finished.\n"));


