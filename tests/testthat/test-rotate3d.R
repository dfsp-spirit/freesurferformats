
test_that("We can rotate a 2D matrix", {
  slice = matrix(seq.int(9), nrow = 3, byrow =TRUE);
  slice_rot_90 = rotate2D(slice, degrees=90);
  slice_rot_180 = rotate2D(slice, degrees=180);
  slice_rot_270 = rotate2D(slice, degrees=270);

  expected_result_90 = matrix(c(7, 4, 1, 8, 5, 2, 9, 6, 3), nrow = 3, byrow = TRUE);
  expected_result_180 = matrix(seq.int(9, 1), nrow = 3, byrow = TRUE);
  expected_result_270 = matrix(c(3, 6, 9, 2, 5, 8, 1, 4, 7), nrow = 3, byrow = TRUE);

  expect_equal(rotate2D(slice, degrees=0), slice);
  expect_equal(rotate2D(slice, degrees=360), slice);
  expect_equal(slice_rot_90, expected_result_90);
  expect_equal(slice_rot_180, expected_result_180);
  expect_equal(slice_rot_270, expected_result_270);

  expect_equal(rotate2D(seq.int(5)), seq.int(5)); # returned as is

  expect_error(rotate2D(slice, degrees = 77)); # degrees not a multiple of 90
  expect_error(rotate2D(array(seq(8), dim=c(2,2,2)))); # 3D array not allowed
})


test_that("We can flip a 2D matrix", {
  slice = matrix(seq.int(9), nrow = 3, byrow =TRUE);

  flipped_h = flip2D(slice, how = 'horizontally');
  flipped_v = flip2D(slice, how = 'vertically');

  expected_h = matrix(c(7, 8, 9, 4, 5, 6, 1, 2, 3), nrow = 3, byrow = TRUE);
  expected_v = matrix(c(3, 2, 1, 6, 5, 4, 9, 8, 7), nrow = 3, byrow = TRUE);

  expect_equal(flipped_h, expected_h);
  expect_equal(flipped_v, expected_v);
  expect_equal(flip2D(slice, how = NULL), slice);

  expect_equal(flip2D(seq.int(5)), seq.int(5)); # returned as is

  expect_error(flip2D(array(seq(8), dim=c(2,2,2)))); # 3D array not allowed
  expect_error(flip2D(slice, how = 'dunno')); # invalid how
})


test_that("We can flip a 3D matrix", {
  vdim = c(1, 2, 3);
  volume = array(seq(6), dim = vdim);

  flipped_1_h = flip3D(volume, axis = 1, how = 'horizontally');
  flipped_1_v = flip3D(volume, axis = 1, how = 'vertically');
  flipped_2_h = flip3D(volume, axis = 2, how = 'h');
  flipped_2_v = flip3D(volume, axis = 2, how = 'v');
  flipped_3_h = flip3D(volume, axis = 3, how = 'horizontally');
  flipped_3_v = flip3D(volume, axis = 3, how = 'vertically');

  # dimensions must never change when flipping values
  expect_equal(dim(flipped_1_h), vdim);
  expect_equal(dim(flipped_1_v), vdim);
  expect_equal(dim(flipped_2_h), vdim);
  expect_equal(dim(flipped_2_v), vdim);
  expect_equal(dim(flipped_3_h), vdim);
  expect_equal(dim(flipped_3_v), vdim);

  expect_error(flip3D(seq.int(5))); # not a matrix
  expect_error(flip3D(volume, how = 'dunno')); # invalid how
  expect_error(flip3D(volume, axis = 4)); # invalid axis
})


test_that("We can rotate a 3D matrix", {
  vdim = c(1, 2, 3);
  volume = array(seq(6), dim = vdim);

  rotated_1_90 = rotate3D(volume, axis = 1, degrees = 90L);
  rotated_1_180 = rotate3D(volume, axis = 1, degrees = 180L);
  rotated_2_90 = rotate3D(volume, axis = 2, degrees = 90L);
  rotated_2_180 = rotate3D(volume, axis = 2, degrees = 180L);
  rotated_3_90 = rotate3D(volume, axis = 3, degrees = 90L);
  rotated_3_180 = rotate3D(volume, axis = 3, degrees = 180L);

  # dimensions may change when rotating values
  expect_equal(dim(rotated_1_90), c(1, 3, 2));
  expect_equal(dim(rotated_1_180), vdim);
  expect_equal(dim(rotated_2_90), c(3, 2, 1));
  expect_equal(dim(rotated_2_180), vdim);
  expect_equal(dim(rotated_3_90), c(2, 1, 3));
  expect_equal(dim(rotated_3_180), vdim);

  expect_error(rotate3D(seq.int(5))); # not a matrix
  expect_error(rotate3D(volume, axis = 'dunno')); # invalid axis
  expect_error(rotate3D(volume, axis = 4)); # invalid axis
  expect_error(rotate3D(volume, degrees = 77)); # invalid degrees
})
