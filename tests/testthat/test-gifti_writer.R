

test_that("We can use the GIFTI writer", {
  outfile = tempfile(fileext = '.gii');
  dataarrays = list(rep(3.1, 3L), matrix(seq(6), nrow=2L));
  gifti_writer(outfile, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'));
  gifti_writer(outfile, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), transform_matrix = list(diag(4), diag(4)));

  expect_error(gifti_writer(123, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'))); # not a valid filepath
  expect_error(gifti_writer(outfile, seq(5), datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'))); # not a valid dataarray: must be list
  expect_error(gifti_writer(outfile, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), transform_matrix = seq(5))); # not a valid transform matrix
  expect_error(gifti_writer(outfile, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), transform_matrix = list('a'=1, 'b'=2))); # not a valid transform matrix
  expect_error(gifti_writer(outfile, dataarrays, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), transform_matrix = list(1, 2, 3))); # not a valid transform matrix list: wrong length
  expect_equal(1L, 1L);
})

