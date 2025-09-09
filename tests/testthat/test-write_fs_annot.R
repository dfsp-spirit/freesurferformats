

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

  # write colortable and have the function auto-fill struct_index
  colortable_df2 = colortable_df;
  colortable_df2$struct_index = NULL;
  write.fs.colortable(tempfile(fileext = ".txt"), colortable_df2);

  # errors should be thrown on invalid params
  expect_error(write.fs.colortable(output_file, "what")); # colortable must be a data.frame, not a string

  # delete vital columns and expect error
  colortable_df2$struct_name = NULL;
  colortable_df2$r = NULL;
  expect_error(write.fs.colortable(output_file, colortable_df2)); # colortable missing columns 'struct_name' and 'r'
})


test_that("An annotation can be written in binary v2 format and read again.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".annot");
  write.fs.annot(output_file, length(annot$vertices), annot$colortable_df, labels_as_colorcodes=annot$label_codes);

  annot2 = read.fs.annot(output_file);

  expect_equal(annot$vertices, annot2$vertices);
  expect_equal(annot, annot2);

  # Cannot specify both labels_as_colorcodes and labels_as_indices_into_colortable
  expect_error(write.fs.annot(output_file, length(annot$vertices), annot$colortable_df, labels_as_colorcodes=annot$label_codes, labels_as_indices_into_colortable=rep(1L, length(annot$vertices))));

  # Give wrong number of labels and expect error
  wrong_length = length(annot$vertices) + 5L;
  expect_error(write.fs.annot(output_file, length(annot$vertices), annot$colortable_df, labels_as_indices_into_colortable=rep(1L, wrong_length)));
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


test_that("An annotation can be written in GIFTI format and read again based on the class method.", {
  skip_if_not_installed('gifti', minimum_version = NULL);
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".annot.gii");
  write.fs.annot.gii(output_file, annot);

  annot2 = read.fs.annot.gii(output_file);

  expect_equal(annot$vertices, annot2$vertices, tolerance = 1e-2);
  expect_equal(annot$label_codes, annot2$label_codes);
  expect_equal(annot$label_names, annot2$label_names);
  expect_equal(annot$hex_colors_rgb, annot2$hex_colors_rgb);
})


test_that("Invalid arguments to write.fs.annot lead to errors.", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  expect_error(write.fs.annot.gii(tempfile(), list("a"=12))); # not an annot
  expect_error(write.fs.annot.gii(123, annot)); # invalid filepath
})


test_that("An annotation can be written in binary v2 format in different ways.", {

  output_file = tempfile(fileext = ".annot");
  write.fs.annot(output_file, num_vertices = 10L, colortable = NULL, labels_as_colorcodes=rep(1L, 10L));

  expect_error(write.fs.annot(output_file, num_vertices = 10L, colortable = NULL, labels_as_colorcodes=rep(1L, 9L))); # vertex count != label count
  expect_error(write.fs.annot(output_file, num_vertices = "no idea", colortable = NULL, labels_as_colorcodes=rep(1L, 10L))); # invalid vertex count
  expect_error(write.fs.annot(output_file, num_vertices = 10L, colortable = NULL, labels_as_indices_into_colortable = rep(1L, 10L))); # colortable must not be NULL when using indices into it

  expect_equal(1L, 1L);
})
