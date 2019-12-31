

test_that("A colortable from an annotation can be written to a text file in FreeSurfer LUT format.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".txt");
  write.fs.colortable(output_file, colortable.from.annot(annot));

  written_colortable = read.fs.colortable(output_file);
  expect_equal(nrow(written_colortable), 36);
  expect_equal(ncol(written_colortable), 6);

  region_unknown = subset(written_colortable, written_colortable$struct_name == "unknown");
  expect_equal(region_unknown$struct_index, 0);
  expect_equal(region_unknown$r, 25);
  expect_equal(region_unknown$g, 5);
  expect_equal(region_unknown$b, 25);
  expect_equal(region_unknown$a, 0);
})


test_that("A colortable can be written to a text file in FreeSurfer LUT format.", {

  colortable_df = data.frame("struct_index"=c(0, 1), "struct_name"=c("struct1", "struct2"), "r"=c(80, 100), "g"=c(50, 40), "b"=c(250, 200), "a"=c(0, 0), stringsAsFactors = FALSE);

  output_file = tempfile(fileext = ".txt");
  write.fs.colortable(output_file, colortable_df);

  written_colortable = read.fs.colortable(output_file);

  expect_equal(ncol(colortable_df), ncol(written_colortable));
  expect_equal(nrow(colortable_df), nrow(written_colortable));
  expect_equal(colortable_df, written_colortable);
})


test_that("An annotation can be written in binary v2 format and read again.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".annot");
  write.fs.annot(output_file, length(annot$vertices), annot$colortable_df, labels_as_colorcodes=annot$label_codes);

  annot2 = read.fs.annot(output_file);

  expect_equal(annot$vertices, annot2$vertices);
  expect_equal(annot, annot2);
})


test_that("An annotation can be written in binary v2 format and read again based on the class method.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".annot");
  write.fs.annot(output_file, fs.annot=annot);

  annot2 = read.fs.annot(output_file);

  expect_equal(annot$vertices, annot2$vertices);
  expect_equal(annot, annot2);
})
