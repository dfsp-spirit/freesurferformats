test_that("A label file can be written and read again", {
  output_file = tempfile();

  # generate data
  vertex_indices = seq(from = 10000, to=20000);

  # write data to file
  write.fs.label(output_file, vertex_indices);

  # load data again and check it
  read_vertex_indices = read.fs.label(output_file);
  expect_equal(vertex_indices, read_vertex_indices);
})


test_that("A label file can be written and read again using the class method", {
  output_file = tempfile();

  # load data
  labelfile = system.file("extdata", "lh.entorhinal_exvivo.label", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label(labelfile, full=TRUE);
  expect_true(is.fs.label(label));
  expect_equal(length(label$vertexdata$vertex_index), 1085);

  # write data to file
  write.fs.label(output_file, label);

  # load data again and check it
  label2 = read.fs.label(output_file, full=TRUE);
  expect_equal(label$vertexdata$vertex_index, label2$vertexdata$vertex_index);
})
