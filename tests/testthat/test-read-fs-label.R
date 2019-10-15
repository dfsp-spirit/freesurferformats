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
})

test_that("Our demo label file can be read with one-based R indices", {
  labelfile = system.file("extdata", "lh.entorhinal_exvivo.label", package = "freesurferformats", mustWork = TRUE);

  label = read.fs.label(labelfile, return_one_based_indices=TRUE);
  known_vertex_count_label = 1085

  # These are the indices from the file, i.e., they are zero-based. We add 1 below.
  v1 = 88791;
  v2 = 88805;
  v1084 = 149163;
  v1085 = 149165;

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(length(label), known_vertex_count_label);
  expect_equal(class(label), "integer");
  expect_true(is.vector(label));

  expect_equal(label[1], v1 + 1);
  expect_equal(label[2], v2 + 1);
  expect_equal(label[1084], v1084 + 1);
  expect_equal(label[1085], v1085 + 1);
})

