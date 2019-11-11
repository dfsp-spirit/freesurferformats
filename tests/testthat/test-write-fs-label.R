test_that("A label file can be written", {
  output_file = tempfile();

  # generate data
  vertex_indices = seq(from = 10000, to=20000);

  # write data to file
  write.fs.label(output_file, vertex_indices);

  # load data again and check it
  read_vertex_indices = read.fs.label(output_file);
  expect_equal(vertex_indices, read_vertex_indices);
})
