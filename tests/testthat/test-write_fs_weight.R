


test_that("One can write and re-read binary weight data", {
  vertex_indices = as.integer(c(1L, 45L, 255L));
  values = c(-0.56, 0.34, 44000.45);


  tmp_file = tempfile(fileext=".w");
  format_written = write.fs.weight(tmp_file, vertex_indices, values);


  res = read.fs.weight(tmp_file);
  expect_equal(res$vertex_indices, vertex_indices);
  expect_equal(res$values, values, tolerance=1e-2);
})


test_that("One can write and re-read ASCII weight data", {
  vertex_indices = as.integer(c(1L, 45L, 255L));
  values = c(-0.56, 0.34, 44000.45);


  tmp_file = tempfile(fileext=".w.asc");
  format_written = write.fs.weight(tmp_file, vertex_indices, values, format = 'asc');

  res = read.fs.weight(tmp_file);
  expect_equal(res$vertex_indices, vertex_indices);
  expect_equal(res$values, values, tolerance=1e-2);
})


test_that("Invalid arguments to write.fs.weight lead to errors", {
  vertex_indices = as.integer(c(1L, 45L, 255L));
  values = c(-0.56, 0.34, 44000.45);
  tmp_file = tempfile(fileext=".w");

  expect_error(write.fs.weight(tmp_file, (vertex_indices - 1L), values)); # vertex index 0 not allowed
  expect_error(write.fs.weight(tmp_file, c(2, 24), values)); # 3 values but only 2 vertex indices
})
