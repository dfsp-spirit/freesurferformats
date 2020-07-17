test_that("Our demo label file can be read with original, zero-based indices", {
  labelfile = system.file("extdata", "lh.entorhinal_exvivo.label", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label(labelfile, return_one_based_indices=FALSE);
  known_vertex_count_label = 1085

  # These are the indices from the file, i.e., they are zero-based.
  v1 = 88791;
  v2 = 88805;
  v1084 = 149163;
  v1085 = 149165;

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(length(label), known_vertex_count_label);
  expect_equal(class(label), "integer");
  expect_true(is.vector(label));

  expect_equal(label[1], v1);
  expect_equal(label[2], v2);
  expect_equal(label[1084], v1084);
  expect_equal(label[1085], v1085);

  label; # call print.fs.label
})


test_that("Our demo label file can be read with one-based R indices", {
  labelfile = system.file("extdata", "lh.entorhinal_exvivo.label", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label(labelfile, return_one_based_indices=TRUE);
  known_vertex_count_label = 1085;

  # These are the indices from the file, i.e., they are zero-based. We add 1 below.
  v1 = 88791;
  v2 = 88805;
  v1084 = 149163;
  v1085 = 149165;

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(length(label), known_vertex_count_label);
  expect_equal(class(label), "integer");
  expect_true(is.vector(label));

  expect_false(is.fs.label(label)); # need to set parameter 'full' to TRUE if you want that, see test below.

  expect_equal(label[1], v1 + 1);
  expect_equal(label[2], v2 + 1);
  expect_equal(label[1084], v1084 + 1);
  expect_equal(label[1085], v1085 + 1);
})


test_that("Our demo label file can be read into an fs.label instance", {
  labelfile = system.file("extdata", "lh.entorhinal_exvivo.label", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label(labelfile, full=TRUE);
  known_vertex_count_label = 1085;

  # These are the indices from the file, i.e., they are zero-based. We add 1 below.
  v1 = 88791;
  v2 = 88805;
  v1084 = 149163;
  v1085 = 149165;

  expect_true(is.fs.label(label));
  expect_false(is.fs.label(list("blah"="label")));

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(nrow(label$vertexdata), known_vertex_count_label);


  expect_equal(label$vertexdata$vertex_index[1], v1 + 1);
  expect_equal(label$vertexdata$vertex_index[2], v2 + 1);
  expect_equal(label$vertexdata$vertex_index[1084], v1084 + 1);
  expect_equal(label$vertexdata$vertex_index[1085], v1085 + 1);
})


test_that("A gifti surface label file can be read", {
  labelfile = system.file("extdata", "tiny_label.gii", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label.gii(labelfile);
  label = read.fs.label.gii(labelfile, label_value = TRUE);
  label2 = read.fs.label(labelfile);
  known_vertex_count_label = 21;

  expect_equal(length(label), known_vertex_count_label);
  expect_equal(length(label2), known_vertex_count_label);
  expect_equal(label, seq.int(50, 70));
  expect_equal(label2, seq.int(50, 70));

  expect_error(read.fs.label.gii(labelfile, label_value = 'dunno'));  # label_value must be numeric
  expect_error(read.fs.label(labelfile, format = 'dunno'));  # invalid format
})

