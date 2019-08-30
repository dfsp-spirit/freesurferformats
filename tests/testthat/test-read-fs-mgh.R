test_that("The dimensions of our demo file are read correctly", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image);

  expect_equal(length(dim(vd)), 4)  # It has 4 dimensions
  expect_equal(dim(vd), c(256, 256, 256, 1))
})
