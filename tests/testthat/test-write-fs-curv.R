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

  expect_error(write.fs.morph(tempfile(fileext="mgz"), data)); # not implemented

})
