test_that("The dimensions of our demo MGZ file are read correctly", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image);

  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  expect_equal(dim(vd), c(256, 256, 256, 1));
})


test_that("Data is flattened when requested", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image, flatten=TRUE);

  expect_equal(class(vd), "integer");
  expect_equal(length((vd)), 16777216);
})


test_that("The gzip status is guessed as expected from a filename", {
  expect_equal(guess.filename.is.gzipped("noway"), FALSE);
  expect_equal(guess.filename.is.gzipped("/there/is/noway"), FALSE);
  expect_equal(guess.filename.is.gzipped("file.tar.gz"), TRUE);
  expect_equal(guess.filename.is.gzipped("relative/path/to/file.tar.gz"), TRUE);
  expect_equal(guess.filename.is.gzipped("file.tar"), FALSE);
  expect_equal(guess.filename.is.gzipped("brain.mgz"), TRUE);
  expect_equal(guess.filename.is.gzipped("brain.mgh"), FALSE);
})

