
test_that("A FreeSurfer transformation matrix can be read from an xfm file.", {

  tf_file = system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE);
  transform = read.fs.transform(tf_file);

  expect_true(is.list(transform));
  expect_true(is.matrix(transform$matrix));
  expect_true(is.character(transform$type));

  expect_equal(ncol(transform$matrix), 4);
  expect_equal(nrow(transform$matrix), 4);

  expect_equal(transform$type, 'Linear');

  expect_error(read.fs.transform(tf_file, format = 'invalid')); # invalid format
})


test_that("A FreeSurfer transformation matrix can be read from a tkregister dat file.", {

  tf_file = system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE);
  transform = read.fs.transform(tf_file);

  expect_true(is.list(transform));
  expect_true(is.matrix(transform$matrix));

  expect_equal(ncol(transform$matrix), 4);
  expect_equal(nrow(transform$matrix), 4);

  expect_error(read.fs.transform(tf_file, format = 'invalid')); # invalid format
})


test_that("A FreeSurfer transformation matrix can be read from an LTA file.", {

  tf_file = system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE);
  transform = read.fs.transform(tf_file);

  expect_true(is.list(transform));
  expect_true(is.matrix(transform$matrix));

  expect_equal(ncol(transform$matrix), 4);
  expect_equal(nrow(transform$matrix), 4);
  expect_equal(length(transform$volumes), 2);

  expect_equal(length(transform$volumes$src$xras), 3);
  expect_equal(length(transform$volumes$src$yras), 3);
  expect_equal(length(transform$volumes$src$zras), 3);
  expect_equal(length(transform$volumes$src$cras), 3);

  expect_equal(length(transform$volumes$dst$xras), 3);
  expect_equal(length(transform$volumes$dst$yras), 3);
  expect_equal(length(transform$volumes$dst$zras), 3);
  expect_equal(length(transform$volumes$dst$cras), 3);

  expect_error(read.fs.transform(tf_file, format = 'invalid')); # invalid format
})


