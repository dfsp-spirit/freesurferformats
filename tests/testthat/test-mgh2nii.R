# Tests for mgh2nii
# These tests are currenlty deactivated because they require the 'oro.nifti' package.

test_that("An oro.nifti instance can be converted into an fs.volume instance", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header=TRUE);

  nii_file = "~/data/tim_only/tim/mri/brain.nii";
  nii = oro.nifti::readNIfTI(nii_file);

  mgh_from_nii = fs.volume.from.oro.nifti(nii);

  expect_equal(dim(mgh_from_nii$data), dim(mgh$data));
  expect_equal(mghheader.vox2ras(mgh_from_nii), mghheader.vox2ras(mgh));

  expect_equal(mgh_from_nii$data, mgh$data);

  expect_equal(mghheader.crs.orientation(mgh), "LIA");
  expect_equal(mghheader.crs.orientation(mgh_from_nii), "LIA");
})
