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
