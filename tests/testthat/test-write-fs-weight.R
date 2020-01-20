


test_that("One can write and re-read weight data", {
  vertex_indices = as.integer(c(1L, 45L, 255L));
  values = c(-0.56, 0.34, 44000.45);


  tmp_file = tempfile(fileext=".w");
  format_written = write.fs.weight(tmp_file, vertex_indices, values);


  res = read.fs.weight(tmp_file);
  expect_equal(res$vertex_indices, vertex_indices);
  expect_equal(res$values, values, tolerance=1e-2);
})
