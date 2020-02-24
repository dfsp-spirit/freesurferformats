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


test_that("The native ras2vox matrix can be computed from a conformed volume", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  expect_true(is.fs.volume(mgh));

  # Run `mri_info --ras2vox <volume>` on the OS command line to find the reference value.
  known_ras2vox = matrix(c(-1, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 127.5005, 79.09527, 98.62726, 1), nrow=4, byrow = FALSE);

  ras2vox = mghheader.ras2vox(mgh$header);
  expect_equal(ras2vox, known_ras2vox, tolerance=1e-4);
})


test_that("The tkregister vox2ras matrix can be computed from a conformed volume", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  expect_true(is.fs.volume(mgh));

  # Run `mri_info --vox2ras-tkr <volume>` on the OS command line to find the reference value.
  known_vox2ras_tkr = matrix(c(-1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 128., -128., 128., 1), nrow=4, byrow = FALSE);

  vox2ras_tkr = mghheader.vox2ras.tkreg(mgh$header);
  expect_equal(vox2ras_tkr, known_vox2ras_tkr, tolerance=1e-4);
})


test_that("The tkregister ras2vox matrix can be computed from a conformed volume", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  expect_true(is.fs.volume(mgh));

  # Run `mri_info --ras2vox-tkr <volume>` on the OS command line to find the reference value.
  known_ras2vox_tkr = matrix(c(-1, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 128., 128., 128., 1), nrow=4, byrow = FALSE);

  ras2vox_tkr = mghheader.ras2vox.tkreg(mgh$header);
  expect_equal(ras2vox_tkr, known_ras2vox_tkr, tolerance=1e-4);
})


test_that("The slice direction and orientation can be computed from full fs.volume", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  expect_true(mghheader.is.conformed(mgh));
  expect_equal(mghheader.primary.slice.direction(mgh), 'coronal');
  expect_equal(mghheader.crs.orientation(mgh), 'LIA');
})


test_that("The slice direction and orientation can be computed from the volume header", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;
  expect_true(mghheader.is.conformed(header));
  expect_equal(mghheader.primary.slice.direction(header), 'coronal');
  expect_equal(mghheader.crs.orientation(header), 'LIA');
})


test_that("The mgh header fields can be initialized from a vox2ras matrix", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;

  vox2ras = mghheader.vox2ras(header);
  new_header = mghheader.update.from.vox2ras(header, vox2ras);   # Update with the matrix it already has, i.e., set basic fields from matrix.
  new_vox2ras = mghheader.vox2ras(new_header);                   # Recompute matrix from set basic fields.

  expect_equal(vox2ras, new_vox2ras);                            # This should lead to the same matrix.
})



