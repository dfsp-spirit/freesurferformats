# Tests for MGH/MGZ to NIFTI conversion (and back)

test_that("An MGH/MGZ file can be converted to NIFTI v1 and read back with matching header", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header = TRUE);

  # Generate NIFTI v1 header from MGH header
  nii_header = nii1header.for.mgh(mgh);
  expect_equal(nii_header$datatype, 2L);   # MRI_UCHAR -> NIFTI UINT8
  expect_equal(nii_header$bitpix, 8L);
  expect_equal(nii_header$dim[1:5], c(4L, 256L, 256L, 256L, 1L));
  expect_equal(nii_header$sform_code, 1L);
  expect_equal(nii_header$qform_code, 1L);
  expect_equal(nii_header$xyzt_units, 10L);
  expect_equal(nii_header$vox_offset, 352.0);

  # Check that the sform rows match the vox2ras matrix
  vox2ras = mghheader.vox2ras(mgh);
  expect_equal(nii_header$srow_x, vox2ras[1, ]);
  expect_equal(nii_header$srow_y, vox2ras[2, ]);
  expect_equal(nii_header$srow_z, vox2ras[3, ]);

  # Check voxel sizes
  expect_equal(nii_header$pix_dim[2], mgh$header$internal$xsize);
  expect_equal(nii_header$pix_dim[3], mgh$header$internal$ysize);
  expect_equal(nii_header$pix_dim[4], mgh$header$internal$zsize);
})

test_that("An MGH volume can be written to NIFTI and read back with matching data", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header = TRUE);

  # Write to NIFTI
  nii_file = tempfile(fileext = ".nii");
  write.fs.volume(nii_file, mgh);
  expect_true(file.exists(nii_file));

  # Read back via oro.nifti and compare header
  skip_if_not_installed("oro.nifti");
  nii_back = read.fs.volume.nii(nii_file, with_header = TRUE);
  expect_equal(dim(nii_back$data), dim(mgh$data));
  expect_equal(mghheader.vox2ras(nii_back), mghheader.vox2ras(mgh));
})

test_that("An MGH volume can be written to gzipped NIFTI and read back with matching data", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header = TRUE);

  # Write to gzipped NIFTI
  nii_gz_file = tempfile(fileext = ".nii.gz");
  write.fs.volume(nii_gz_file, mgh);
  expect_true(file.exists(nii_gz_file));

  # Read back via oro.nifti and compare
  skip_if_not_installed("oro.nifti");
  nii_back = read.fs.volume.nii(nii_gz_file, with_header = TRUE);
  expect_equal(dim(nii_back$data), dim(mgh$data));
  expect_equal(mghheader.vox2ras(nii_back), mghheader.vox2ras(mgh));
})

test_that("nii1header.for.mgh works when given a filepath string", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  nii_header = nii1header.for.mgh(mgh_file);
  expect_equal(nii_header$sform_code, 1L);
  expect_equal(nii_header$qform_code, 1L);
})

test_that("nii1header.for.mgh errors on invalid input", {
  expect_error(nii1header.for.mgh(list()), "must be an fs.volume instance");
})

test_that("write.fs.volume errors on invalid file extension", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header = TRUE);
  expect_error(write.fs.volume(tempfile(fileext = ".bad"), mgh),
               "Invalid file extension");
})

test_that("write.fs.volume can write back to MGH format", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header = TRUE);

  out_file = tempfile(fileext = ".mgz");
  write.fs.volume(out_file, mgh);
  expect_true(file.exists(out_file));

  mgh_back = read.fs.mgh(out_file, with_header = TRUE);
  expect_equal(dim(mgh_back$data), dim(mgh$data));
  expect_equal(mghheader.vox2ras(mgh_back), mghheader.vox2ras(mgh));
})

# Legacy test: NIFTI -> MGH conversion (read.fs.volume.nii)
test_that("An oro.nifti instance can be converted into an fs.volume instance", {
  mgh_file = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(mgh_file, with_header=TRUE);

  nii_file = "~/data/subject1_only/subject1/mri/brain.nii";
  if(! file.exists(nii_file)) { skip("Test data missing."); }
  mgh_from_nii = read.fs.volume.nii(nii_file, with_header=TRUE);

  expect_equal(dim(mgh_from_nii$data), dim(mgh$data));
  expect_equal(mghheader.vox2ras(mgh_from_nii), mghheader.vox2ras(mgh));

  expect_equal(mghheader.crs.orientation(mgh), "LIA");
  expect_equal(mghheader.crs.orientation(mgh_from_nii), "LIA");

  # Check required rotation empirically. We should be able to get this from the NIFTI spec, but
  # it is not very clear to me. See Q15 here: https://nifti.nimh.nih.gov/nifti-1/documentation/faq#Q3
  # And see the state of NIFTI support in FS, btw: https://nifti.nimh.nih.gov/nifti-1/support/FreeSurferandNIfTI1
  mghdata = drop(mgh$data);
  niidata = drop(mgh_from_nii$data);

  # The following transformation results in the correct orientation for our test volume.
  # It was found empirically, and this may or may not apply to other volumes. This is WIP.
  # Note: check the 'reorient' parameter to oro.nifti::readNIfTI, which is enabled by default. It may be responsible for the need to rotate.
  rotated_nii_data = rotate3D(niidata, 1);

  expect_equal(rotated_nii_data, mghdata);
  expect_true(all.equal(rotate3D(niidata, 1), mghdata));
})

