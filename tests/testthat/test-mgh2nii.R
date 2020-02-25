# Tests for mgh2nii
# These tests are currenlty deactivated because they require the 'oro.nifti' and 'fsbrain' packages.

# test_that("An oro.nifti instance can be converted into an fs.volume instance", {
#   mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
#   mgh = read.fs.mgh(mgh_file, with_header=TRUE);
#
#   nii_file = "~/data/tim_only/tim/mri/brain.nii";
#   nii = oro.nifti::readNIfTI(nii_file);
#
#   mgh_from_nii = fs.volume.from.oro.nifti(nii);
#
#
#   expect_equal(dim(mgh_from_nii$data), dim(mgh$data));
#   expect_equal(mghheader.vox2ras(mgh_from_nii), mghheader.vox2ras(mgh));
#
#   expect_equal(mghheader.crs.orientation(mgh), "LIA");
#   expect_equal(mghheader.crs.orientation(mgh_from_nii), "LIA");
#
#   # Check required rotation empirically. We should be able to get this from the NIFTI spec, but
#   # it is not very clear to me. See Q15 here: https://nifti.nimh.nih.gov/nifti-1/documentation/faq#Q3
#   # And see the state of NIFTI support in FS, btw: https://nifti.nimh.nih.gov/nifti-1/support/FreeSurferandNIfTI1
#   mghdata = drop(mgh$data);
#   niidata = drop(mgh_from_nii$data);
#
#   # The following transformation results in the correct orientation for our test volume.
#   # It was found empirically, and this may or may not apply to other volumes. This is WIP.
#   rotated_nii_data = fsbrain::rotate3D(niidata, 1);
#
#   expect_equal(rotated_nii_data, mghdata);
#
#
#   #all.equal(fsbrain::rotate3D(niidata, 1), mghdata);     # TRUE
#   #all.equal(fsbrain::flip3D(niidata, 1), mghdata);       # FALSE
#
#
# })
