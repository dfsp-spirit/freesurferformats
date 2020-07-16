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
  expect_equal(header$mr_params, c(0,0,0,0,0));
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
  expect_equal(header$mr_params, c(0,0,0,0,0));
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
  expect_equal(header$mr_params, c(0,0,0,0,0));
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


test_that("Logical data can be written and re-read from MGH and MGZ files", {
  # This test is very similar to the last one, but it uses MGZ instead of MGH.
  output_file_mgh = tempfile(fileext = ".mgh");
  output_file_mgz = tempfile(fileext = ".mgz");

  # generate data
  data_int = sample(0:1, 2000, replace=TRUE);
  data_logical = as.logical(data_int);

  # write data to files, re-read and check
  write.fs.mgh(output_file_mgh, data_logical);
  mgh = read.fs.mgh(output_file_mgh, with_header = TRUE, drop_empty_dims = TRUE);
  expect_equal(mgh$data, data_int);
  expect_equal(mgh$header$dtype, translate.mri.dtype("MRI_UCHAR"));


  write.fs.mgh(output_file_mgz, data_logical);
  mgz = read.fs.mgh(output_file_mgz, with_header = TRUE, drop_empty_dims = TRUE);
  expect_equal(mgz$data, data_int);
  expect_equal(mgz$header$dtype, translate.mri.dtype("MRI_UCHAR"));
})


test_that("Improper use of write.fs.mgh leads to errors", {
  data_int = sample(0:1, 2000, replace=TRUE);
  filepath = tempfile(fileext = '.mgh');

  expect_error(write.fs.mgh(123, data_int)); # 123 is not a valid file path (not a string)
  expect_error(write.fs.mgh(filepath, data_int, mr_params = c(0., 0))); # invalid length of mr_params
  expect_error(write.fs.mgh(filepath, data_int, mr_params = "what"));   # mr_params must be double
  expect_error(write.fs.mgh(filepath, data_int, vox2ras_matrix = "what"));   # vox2ras_matrix must be double matrix
  expect_error(write.fs.mgh(filepath, data_int, vox2ras_matrix = matrix(seq(6), nrow=2)));   # vox2ras_matrix must be a 4x4 matrix
  expect_error(write.fs.mgh(filepath, data_int, mri_dtype = 3));   # mri_dtype must be a character string
  expect_error(write.fs.mgh(filepath, data_int, mri_dtype = "no such MRI dtype I guess"));   # mri_dtype must be valid
})

