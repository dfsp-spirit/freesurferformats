
test_that("We can rotate a 2D matrix", {
  slice = matrix(seq.int(9), nrow = 3, byrow =TRUE);
  slice_rot_90 = rotate2D(slice, degrees=90);
  slice_rot_180 = rotate2D(slice, degrees=90);
  slice_rot_270 = rotate2D(slice, degrees=90);

  expected_result_90 = matrix(c(7, 4, 1, 8, 5, 2, 9, 6, 3), nrow = 3, byrow = TRUE);
  expected_result_180 = matrix(seq.int(9, 1), nrow = 3, byrow = TRUE);
  expected_result_270 = matrix(c(3, 6, 9, 2, 5, 8, 1, 4, 7), nrow = 3, byrow = TRUE);

  expect_equal(rotate2D(slice, degrees=0), slice);
  expect_equal(rotate2D(slice, degrees=360), slice);
  expect_equal(slice_rot_90, expected_result_90);
  expect_equal(slice_rot_180, expected_result_180);
  expect_equal(slice_rot_270, expected_result_270);

  expect_error(rotate2D(seq.int(5))); # not a matrix
  expect_error(rotate2D(slice, degrees = 77)); # degrees not a multiple of 90
})


test_that("We can flip a 2D matrix", {
  slice = matrix(seq.int(9), nrow = 3, byrow =TRUE);

  flipped_h = flip2D(slice, how = 'horizontally');
  flipped_v = flip2D(slice, how = 'vertically');

  expected_h = matrix(c(7, 8, 9, 4, 5, 6, 1, 2, 3), nrow = 3, byrow = TRUE);
  expected_v = matrix(c(3, 2, 1, 6, 5, 4, 9, 8, 7), nrow = 3, byrow = TRUE);

  expect_equal(flipped_h, expected_h);
  expect_equal(flipped_v, expected_v);

  expect_error(flip2D(seq.int(5))); # not a matrix
  expect_error(flip2D(slice, how = 'dunno')); # invalid how
})


