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

  format_written = write.fs.morph(tempfile(fileext="whatever.gz"), data);
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
  expect_equal(write.fs.morph(tempfile(fileext="gz"), data), "curv");
})


test_that("Morphometry file formats are derived from file names correctly", {
  data_length = 149244;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  expect_equal(fs.get.morph.file.format.from.filename("/blah.mgh"), "mgh");
  expect_equal(fs.get.morph.file.format.from.filename("/blah.mgz"), "mgz");
  expect_equal(fs.get.morph.file.format.from.filename("/blah.whatever"), "curv");
  expect_equal(fs.get.morph.file.format.from.filename("/blah.whatever.gz"), "curv");
})



test_that("Morphometry file extensions are derived from formats correctly when writing morph data files", {
  expect_equal(fs.get.morph.file.ext.for.format("mgh"), ".mgh");
  expect_equal(fs.get.morph.file.ext.for.format("mgz"), ".mgz");
  expect_equal(fs.get.morph.file.ext.for.format("curv"), "");
  expect_error(fs.get.morph.file.ext.for.format("nosuchformat"));
})


test_that("Writing morph files and re-reading the data works for different formats", {

  # generate data
  data_length = 149;
  data = rep(1.25, data_length);
  data[5] = 3.5;

  # FreeSurfer ASCII curv format
  asc_file = tempfile(fileext = ".asc");
  write.fs.morph.asc(asc_file, data);
  asc_data = read.fs.morph.asc(asc_file);
  asc_data2 = read.fs.curv(asc_file);
  expect_equal(length(asc_data), 149L);
  expect_equal(length(asc_data2), 149L);
  expect_equal(asc_data[5], 3.5);
  expect_equal(asc_data2[5], 3.5);

  # simple text format: one value per line
  txt_file = tempfile(fileext = ".txt");
  write.fs.morph.txt(txt_file, data);
  txt_data = read.fs.morph.txt(txt_file);
  txt_data2 = read.fs.curv(txt_file);
  expect_equal(length(txt_data), 149L);
  expect_equal(length(txt_data2), 149L);
  expect_equal(txt_data[5], 3.5);
  expect_equal(txt_data2[5], 3.5);
})


test_that("Trying to write invalid data with write.fs.curv leads to erroes", {
  filepath = tempfile(fileext = ".asc");
  data = rep(0.9, 100L);
  expect_error(write.fs.morph.asc(filepath, data, coords = matrix(seq(300), ncol = 4))); # wrong number of columns in coords
})

