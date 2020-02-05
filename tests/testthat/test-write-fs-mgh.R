test_that("An one-dimensional MGH file of double values can be written", {
  output_file = tempfile();

  # generate data
  data_length = 149244;
  data = rep(1.25, data_length);
  data_array = array(data);

  # write data to file
  write.fs.mgh(output_file, data_array);

  # load data again and check it
  mgh = read.fs.mgh(output_file, with_header=TRUE);
  read_data = mgh$data
  header = mgh$header
  expect_equal(header$mr_params, c(0,0,0,0));
  expect_equal(header$dtype, 3); #MRI_FLOAT
  expect_equal(header$ras_good_flag, -1L);

  # Check data dimensions: we should get 4 dimensions back. Since we wrote a vector of data only, the last 3 should be 1.
  expect_equal(typeof(read_data), "double");
  expect_equal(length(dim(read_data)), 4L);
  expect_equal((dim(read_data)[1]), data_length);
  expect_equal((dim(read_data)[2]), 1L);
  expect_equal((dim(read_data)[3]), 1L);
  expect_equal((dim(read_data)[4]), 1L);

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


test_that("An one-dimensional uncompressed MGH file of integer values can be written", {
  output_file = tempfile(fileext = ".mgh");

  # generate data
  data_length = 111111L;
  data = rep.int(1L, data_length);
  data[10000:20000] = 0L;         # Set some of the values to zero.
  expect_true(is.integer(data));
  expect_equal(range(data), c(0L, 1L));

  # write data to file
  write.fs.mgh(output_file, data);

  # load data again and check it
  mgh = read.fs.mgh(output_file, with_header=TRUE);
  read_data = mgh$data;
  header = mgh$header;
  expect_equal(header$mr_params, c(0,0,0,0));
  expect_equal(header$dtype, 1); #MRI_INT
  expect_equal(header$ras_good_flag, -1L);

  # Check data dimensions: we should get 4 dimensions back. Since we wrote a vector of data only, the last 3 should be 1.
  expect_equal(length(dim(read_data)), 4L);
  expect_equal(typeof(read_data), "integer");
  expect_equal((dim(read_data)[1]), data_length);
  expect_equal((dim(read_data)[2]), 1L);
  expect_equal((dim(read_data)[3]), 1L);
  expect_equal((dim(read_data)[4]), 1L);

  # Check the data values
  expect_equal(length(data), length(as.vector(read_data)));
  expect_equal(data_length, length(as.vector(read_data)));
  expect_equal(data, as.vector(read_data));
  expect_equal(range(read_data), c(0L, 1L));
})


test_that("An one-dimensional compressed MGZ file of integer values can be written", {
  # This test is very similar to the last one, but it uses MGZ instead of MGH.
  output_file = tempfile(fileext = ".mgz");

  # generate data
  data_length = 111111L;
  data = rep.int(1L, data_length);
  data[10000:20000] = 0L;         # Set some of the values to zero.
  expect_true(is.integer(data));
  expect_equal(range(data), c(0L, 1L));

  # write data to file
  write.fs.mgh(output_file, data);

  # load data again and check it
  mgh = read.fs.mgh(output_file, with_header=TRUE);
  read_data = mgh$data;
  header = mgh$header;
  expect_equal(header$mr_params, c(0,0,0,0));
  expect_equal(header$dtype, 1); #MRI_INT
  expect_equal(header$ras_good_flag, -1L);

  # Check data dimensions: we should get 4 dimensions back. Since we wrote a vector of data only, the last 3 should be 1.
  expect_equal(length(dim(read_data)), 4L);
  expect_equal(typeof(read_data), "integer");
  expect_equal((dim(read_data)[1]), data_length);
  expect_equal((dim(read_data)[2]), 1L);
  expect_equal((dim(read_data)[3]), 1L);
  expect_equal((dim(read_data)[4]), 1L);

  # Check the data values
  expect_equal(length(data), length(as.vector(read_data)));
  expect_equal(data_length, length(as.vector(read_data)));
  expect_equal(data, as.vector(read_data));
  expect_equal(range(read_data), c(0L, 1L));
})
