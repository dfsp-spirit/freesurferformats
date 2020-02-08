# Tests for mghheader functions

test_that("The native vox2ras matrix can be computed from a conformed volume", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  expect_true(is.fs.volume(mgh));

  # Run `mri_info --vox2ras <volume>` on the OS command line to find the reference value.
  known_vox2ras = matrix(c(-1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 127.5005, -98.62726, 79.09527, 1), nrow=4, byrow = FALSE);

  vox2ras = mghheader.vox2ras(mgh$header);
  expect_equal(vox2ras, known_vox2ras, tolerance=1e-4);
})
