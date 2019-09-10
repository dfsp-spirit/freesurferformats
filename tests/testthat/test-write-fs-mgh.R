test_that("An one-dimensional MGH file can be written", {
  output_file = tempfile();

  # generate data
  data_length = 149244;
  data = array(rep(1.25, data_length));

  # write data to file
  write.fs.mgh(output_file, data);

  # load data again and check it
  read_data = read.fs.mgh(output_file);
  expect_equal(data, read_data);
})
