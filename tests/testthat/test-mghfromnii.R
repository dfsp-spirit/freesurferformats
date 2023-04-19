# Tests for nifti2mgh

testthat::test_that("An oro.nifti instance can be converted into an fs.volume instance", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header=TRUE);

  nii_file = "~/data/subject1_only/subject1/mri/brain.nii";
  if(! file.exists(nii_file)) { skip("Test data missing."); }
  mgh_from_nii = read.fs.volume.nii(nii_file, with_header=TRUE);

  testthat::expect_equal(dim(mgh_from_nii$data), dim(mgh$data));
  testthat::expect_equal(mghheader.vox2ras(mgh_from_nii), mghheader.vox2ras(mgh));

  testthat::expect_equal(mghheader.crs.orientation(mgh), "LIA");
  testthat::expect_equal(mghheader.crs.orientation(mgh_from_nii), "LIA");

  # Check required rotation empirically. We should be able to get this from the NIFTI spec, but
  # it is not very clear to me. See Q15 here: https://nifti.nimh.nih.gov/nifti-1/documentation/faq#Q3
  # And see the state of NIFTI support in FS, btw: https://nifti.nimh.nih.gov/nifti-1/support/FreeSurferandNIfTI1
  mghdata = drop(mgh$data);
  niidata = drop(mgh_from_nii$data);

  # The following transformation results in the correct orientation for our test volume.
  # It was found empirically, and this may or may not apply to other volumes. This is WIP.
  # Note: check the 'reorient' parameter to oro.nifti::readNIfTI, which is enabled by default. It may be responsible for the need to rotate.
  rotated_nii_data = rotate3D(niidata, 1);

  testthat::expect_equal(rotated_nii_data, mghdata);
  testthat::expect_true(all.equal(rotate3D(niidata, 1), mghdata));
})

