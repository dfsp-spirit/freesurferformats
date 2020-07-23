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

  # check some errors
  expect_error(write.fs.label(output_file, c(-3, 5, 6))); # negative index
  expect_error(write.fs.label(output_file, c(0, 5, 6), indices_are_one_based = TRUE)); # zero index in 1-based data
  expect_error(write.fs.label(output_file, c(1, 5, 6), vertex_data = rep(1.1, 4L))); # 3 vertex indices, but vertex_data for 4.

  vertex_coords_brk = matrix(rep(1.0, 4L * 3), nrow= 4L);
  expect_error(write.fs.label(output_file, c(1, 5, 6), vertex_coords = vertex_coords_brk)); # 3 vertex indices, but vertex_coords for 4.
})


test_that("A GIFTI label file can be written and read again", {
  label = c(1L, 23L, 44L); # the positive vertex indices
  outfile = tempfile(fileext=".gii");
  write.fs.label.gii(outfile, label, 50L);

  label_pos_read = read.fs.label.gii(outfile, label_value=1L);
  expect_equal(length(label_pos_read), 3L);
  expect_equal(label, label_pos_read);

  label_neg_read = read.fs.label.gii(outfile, label_value=0L);
  expect_equal(length(label_neg_read), 47L);

  expect_error(write.fs.label.gii(1234, label, 50L)); # filepath invalid: not char
  expect_error(write.fs.label.gii(1234, label, 30L)); # max vertex index > num vertices
})

