

test_that("An colormap from an annotation can be written to a text file in FreeSurfer LUT format.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile();
  write.fs.colormap(output_file, annot);

  written_cmap_df = read.table(output_file, sep=" ", stringsAsFactors = FALSE);
  expect_equal(nrow(written_cmap_df), 36);
  expect_equal(ncol(written_cmap_df), 6);
})
