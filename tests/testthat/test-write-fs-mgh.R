test_that("An one-dimensional MGH file can be written", {
  output_file = tempfile();

  # generate data
  data_length = 149244;
  data = rep(1.25, data_length);
  data_array = array(data);

  # write data to file
  write.fs.mgh(output_file, data_array);

  # load data again and check it
  read_data = read.fs.mgh(output_file);

  # Check data dimensions: we should get 4 dimensions back. Since we wrote a vector of data only, the last 3 should be 1.
  expect_equal(length(dim(read_data)), 4);
  expect_equal((dim(read_data)[1]), data_length);
  expect_equal((dim(read_data)[2]), 1);
  expect_equal((dim(read_data)[3]), 1);
  expect_equal((dim(read_data)[4]), 1);

  # Check the data values
  expect_equal(length(data), length(as.vector(read_data)));
  expect_equal(data_length, length(as.vector(read_data)));
  expect_equal(data, as.vector(read_data));
})

test_that("A three-dimensional MGH file with realistic size can be written", {
  output_file = tempfile();

  # generate data
  data_array = array(data=rep(1, 256*256*256), dim=c(256,256,256));

  # write data to file
  write.fs.mgh(output_file, data_array);

  # load data again and check it
  read_data = read.fs.mgh(output_file);

  # Check data dimensions: we should get 4 dimensions back. Since we wrote a vector of data only, the last 3 should be 1.
  expect_equal(length(dim(read_data)), 4);
  expect_equal((dim(read_data)[1]), 256);
  expect_equal((dim(read_data)[2]), 256);
  expect_equal((dim(read_data)[3]), 256);
  expect_equal((dim(read_data)[4]), 1);

  # Check the data values
  expect_equal(length(data_array), length(read_data));
  expect_equal(data_array, read_data[,,,1]);
})
