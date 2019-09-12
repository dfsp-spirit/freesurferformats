test_that("The dimensions of our demo MGZ file are read correctly", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image);

  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  expect_equal(dim(vd), c(256, 256, 256, 1));
})


test_that("The header can be read", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  ret = read.fs.mgh(brain_image, with_header=TRUE);
  expect_equal(class(ret), "list");

  header = ret$header;
  expect_equal(class(header), "list");
  expect_equal(header$dtype, 0);
  expect_equal(header$dof, 0);
  expect_equal(header$ras_good_flag, 1);
  expect_equal(length(header$delta), 3);
  expect_equal(header$delta, c(1, 1, 1), tolerance=1e-2);
  expect_equal(length(header$Mdc), 9);
  expect_equal(header$Mdc, c(-1,0,0,0,0,-1,0,1,0), tolerance=1e-2);
  expect_equal(length(header$Pxyz_c), 3);
  expect_equal(header$Pxyz_c, c(-0.5, 29.4, -48.9), tolerance=1e-2);
  expect_equal(header$voldim, c(256, 256, 256, 1));


  vd = ret$data;
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

