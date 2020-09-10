test_that("Our demo curv file can be read using read.fs.curv", {
    curvfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE);
    ct = read.fs.curv(curvfile);
    known_vertex_count = 149244;

    expect_equal(class(ct), "numeric");
    expect_equal(length(ct), known_vertex_count);
})


test_that("Our demo curv file can be read using read.fs.morph", {
  curvfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE);
  ct = read.fs.morph(curvfile);
  known_vertex_count = 149244;

  expect_equal(class(ct), "numeric");
  expect_equal(length(ct), known_vertex_count);
})


test_that("Our demo morphometry data MGZ file can be read using read.fs.morph", {
  morphfile = system.file("extdata", "lh.curv.fwhm10.fsaverage.mgz", package = "freesurferformats", mustWork = TRUE);
  curv = read.fs.morph(morphfile);
  known_vertex_count_fsaverage = 163842;

  expect_equal(class(curv), "numeric");
  expect_equal(length(curv), known_vertex_count_fsaverage);
})


test_that("Our demo morphometry data GIFTI file can be read using read.fs.morph", {
  morphfile = system.file("extdata", "tiny_morph.gii", package = "freesurferformats", mustWork = TRUE);
  morph_data = read.fs.morph(morphfile);

  expect_equal(length(morph_data), 50L);
})


test_that("Invalid arguments to read.fs.curv lead to errors", {
  filepath = tempfile(fileext=".curv");
  expect_error(read.fs.curv(filepath, format = "invalid")); # invalid format
  expect_error(read.fs.morph(filepath, format = "invalid")); # invalid format
})

