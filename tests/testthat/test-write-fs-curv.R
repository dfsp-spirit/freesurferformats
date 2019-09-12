test_that("A curv file can be written", {
  output_file = tempfile();

  # generate data
  data_length = 149244;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  # write data to file
  write.fs.curv(output_file, data);

  # load data again and check it
  read_data = read.fs.curv(output_file);
  expect_equal(data, read_data);
})


test_that("Writing morph files in a format based on the filename works", {

  # generate data
  data_length = 149244;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  # write data to files
  format_written = write.fs.morph(tempfile(fileext="mgh"), data);
  expect_equal(format_written, "mgh");

  format_written = write.fs.morph(tempfile(fileext="whatever"), data);
  expect_equal(format_written, "curv");

  format_written = write.fs.morph(tempfile(fileext="mgz"), data);
  expect_equal(format_written, "mgz");
})


test_that("Morphometry file formats are derived from file names correctly", {
  data_length = 149244;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  expect_equal(write.fs.morph(tempfile(fileext="mgh"), data), "mgh");
  expect_equal(write.fs.morph(tempfile(fileext="mgz"), data), "mgz");
  expect_equal(write.fs.morph(tempfile(fileext=""), data), "curv");
})


test_that("Morphometry file formats are derived from file names correctly", {
  data_length = 149244;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  expect_equal(fs.get.morph.file.format.from.filename("/blah.mgh"), "mgh");
  expect_equal(fs.get.morph.file.format.from.filename("/blah.mgz"), "mgz");
  expect_equal(fs.get.morph.file.format.from.filename("/blah.whatever"), "curv");
})



test_that("Morphometry file extensions are derived from formats correctly when writing morph data files", {
  expect_equal(fs.get.morph.file.ext.for.format("mgh"), ".mgh");
  expect_equal(fs.get.morph.file.ext.for.format("mgz"), ".mgz");
  expect_equal(fs.get.morph.file.ext.for.format("curv"), "");
  expect_error(fs.get.morph.file.ext.for.format("nosuchformat"));
})


