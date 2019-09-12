test_that("The dimensions of our demo 3D MGZ file are read correctly", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image);

  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  expect_equal(dim(vd), c(256, 256, 256, 1));
})


test_that("The demo 1D MGZ file is read correctly", {
  morph_file = system.file("extdata", "lh.curv.fwhm10.fsaverage.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(morph_file);

  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  num_vers_fsaverage = 163842
  expect_equal(dim(vd), c(num_vers_fsaverage, 1, 1, 1));
})


test_that("The header can be read", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  ret = read.fs.mgh(brain_image, with_header=TRUE);
  expect_equal(class(ret), "list");

  header = ret$header;
  expect_equal(class(header), "list");
  expect_equal(header$dtype, 0);  # MRI_UCHAR
  expect_equal(header$dof, 0);
  expect_equal(header$ras_good_flag, 1);
  expect_equal(length(header$delta), 3);
  expect_equal(header$delta, c(1, 1, 1), tolerance=1e-2);
  expect_equal(length(header$Mdc), 9);
  expect_equal(header$Mdc, matrix(c(-1,0,0,0,0,-1,0,1,0), nrow=3), tolerance=1e-2);
  expect_equal(length(header$Pxyz_c), 3);
  expect_equal(header$Pxyz_c, c(-0.5, 29.4, -48.9), tolerance=1e-2);
  expect_equal(header$voldim, c(256, 256, 256, 1));
  expect_equal(length(header$mr_params), 4);
  expect_equal(header$mr_params, c(2300.000000, 0.157080, 2.010000, 900.000000), tolerance=1e-2);
  expect_equal(length(header$D), 9);  # 3x3
  expect_equal(length(header$Pcrs_c), 3); # 3x1
  expect_equal(length(header$Pxyz_0), 3); # 3x1
  expect_equal(length(header$vox2ras_matrix), 16);
  expect_equal(header$vox2ras_matrix, matrix(c(-1,0,0,0,  0,0,-1,0,  0,1,0,0,  127.5,-98.6273,79.0953,1.000), nrow=4, byrow = FALSE), tolerance=1e-2);


  vd = ret$data;
  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  expect_equal(typeof(vd), "integer");
  expect_equal(dim(vd), c(256, 256, 256, 1));
  expect_equal(vd[80,80,80,1], 110); # value of voxel 80,80,80 must be 110 (known from ref. implementation)
  expect_equal(vd[100,100,100,1], 77);
  expect_equal(vd[100,100,80,1], 105);
})


test_that("Data is flattened when requested", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  vd = read.fs.mgh(brain_image, flatten=TRUE);

  expect_equal(class(vd), "integer");
  expect_equal(length((vd)), 16777216);
})


test_that("The gzip status is guessed as expected from a filename", {
  expect_equal(guess.filename.is.gzipped("noway"), FALSE);
  expect_equal(guess.filename.is.gzipped("/there/is/noway"), FALSE);
  expect_equal(guess.filename.is.gzipped("file.tar.gz"), TRUE);
  expect_equal(guess.filename.is.gzipped("relative/path/to/file.tar.gz"), TRUE);
  expect_equal(guess.filename.is.gzipped("file.tar"), FALSE);
  expect_equal(guess.filename.is.gzipped("brain.mgz"), TRUE);
  expect_equal(guess.filename.is.gzipped("brain.mgh"), FALSE);
})


test_that("A real MGH can be read, rewritten, read again, and the data and header are preserved as expected.", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  ret_orig = read.fs.mgh(brain_image, with_header=TRUE);
  expect_equal(class(ret_orig), "list");

  new_copy = tempfile(fileext="mgz");
  write.fs.mgh(new_copy, ret_orig$data, vox2ras_matrix = ret_orig$header$vox2ras_matrix, mr_params=ret_orig$header$mr_params)

  ret = read.fs.mgh(new_copy, with_header=TRUE);
  header = ret$header;
  expect_equal(class(header), "list");
  expect_equal(header$dtype, 1);  # IMPORTANT: The data type will have changed from MRI_UCHAR to MRI_INTEGER. This is fine with us for now.
  expect_equal(header$dof, 0);
  expect_equal(header$ras_good_flag, 1);
  expect_equal(length(header$delta), 3);
  expect_equal(header$delta, c(1, 1, 1), tolerance=1e-2);
  expect_equal(length(header$Mdc), 9);
  expect_equal(header$Mdc, matrix(c(-1,0,0,0,0,-1,0,1,0), nrow=3), tolerance=1e-2);
  expect_equal(length(header$D), 9);  # 3x3
  expect_equal(length(header$Pcrs_c), 3); # 3x1
  expect_equal(length(header$Pxyz_0), 3); # 3x1
  expect_equal(header$Pxyz_c, c(-0.5, 29.4, -48.9), tolerance=1e-2);
  expect_equal(header$voldim, c(256, 256, 256, 1));
  expect_equal(length(header$mr_params), 4);
  expect_equal(header$mr_params, c(2300.000000, 0.157080, 2.010000, 900.000000), tolerance=1e-2);
  expect_equal(header$vox2ras_matrix, matrix(c(-1,0,0,0,  0,0,-1,0,  0,1,0,0,  127.5,-98.6273,79.0953,1.000), nrow=4, byrow = FALSE), tolerance=1e-2);


  vd = ret$data;
  expect_equal(class(vd), "array");
  expect_equal(length(dim(vd)), 4);  # It has 4 dimensions
  expect_equal(dim(vd), c(256, 256, 256, 1));
  expect_equal(vd[80,80,80,1], 110); # value of voxel 80,80,80 must be 110 (known from ref. implementation)
  expect_equal(vd[100,100,100,1], 77);
  expect_equal(vd[100,100,80,1], 105);
})
