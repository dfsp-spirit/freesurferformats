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

  header_noras = mgh$header;
  header_noras$ras_good_flag = 0L;
  expect_false(mghheader.is.conformed(header_noras));
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


test_that("The RAS coordinate of the center voxel (CRAS) can be computed from the RAS coordinate for the first voxel (P0 RAS).", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;

  vox2ras = mghheader.vox2ras(header);
  first_voxel_CRS = c(1L, 1L, 1L);
  first_voxel_RAS = vox2ras[,4][0:3];  # the P0 RAS
  known_first_voxel_RAS = c(127.5, -98.6273, 79.0953);   # known from: `mri_info --p0  path/to/brain.mgz` (in system shell)
  expect_equal(first_voxel_RAS, known_first_voxel_RAS, tolerance=1e-2);

  center_RAS = mghheader.centervoxelRAS.from.firstvoxelRAS(header, first_voxel_RAS);  # center RAS is also known as 'CRAS'.

  known_center_RAS = c(-0.499954, 29.3727, -48.9047);     # known from: `mri_info --cras  path/to/brain.mgz` (in system shell)

  expect_equal(center_RAS, known_center_RAS, tolerance=1e-2);
})


test_that("MGH header can be checked for valid RAS", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;

  expect_true(mghheader.is.ras.valid(header));
  expect_true(mghheader.is.ras.valid(mgh));

  expect_false(mghheader.is.ras.valid(NULL));
  expect_false(mghheader.is.ras.valid("what"));

  header_noras = header;
  header_noras$ras_good_flag = 0L;
  expect_false(mghheader.is.ras.valid(header_noras));

  header_noras$ras_good_flag = NULL;
  expect_false(mghheader.is.ras.valid(header_noras));
})


test_that("MGH header can be used to compute tkreg2scanner", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;

  tkm = mghheader.tkreg2scanner(header);
  tkm2 = mghheader.tkreg2scanner(mgh);

  header_noras = header;
  header_noras$ras_good_flag = 0L;
  expect_error(mghheader.tkreg2scanner(header_noras));
})


test_that("MGH header can be used to compute scanner2tkreg", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;

  tkm = mghheader.scanner2tkreg(header);
  tkm2 = mghheader.scanner2tkreg(mgh);

  header_noras = header;
  header_noras$ras_good_flag = 0L;
  expect_error(mghheader.scanner2tkreg(header_noras));
})


test_that("Two MGH headers can be used to compute vox2vox", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  header = mgh$header;
  header2 = header;

  v2v = mghheader.vox2vox(header, header2);
  v2v = mghheader.vox2vox(mgh, mgh);

  header_noras = header;
  header_noras$ras_good_flag = 0L;
  expect_error(mghheader.vox2vox(header_noras, header));
  expect_error(mghheader.vox2vox(header, header_noras));
  expect_error(mghheader.vox2vox(header_noras, header_noras));
})


test_that("Transformation matrices can be adapted to 0-based and 1-based indices", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  vox2ras = mghheader.vox2ras(mgh$header);

  vox2ras_for_1based = sm0to1(vox2ras);
  vox2ras_for_0based = sm1to0(vox2ras_for_1based);
  expect_equal(vox2ras, vox2ras_for_0based, tolerance=1e-2);
})


test_that("Transformation from surface RAS to RAS and back works", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  mghheader = mgh$header;

  # get surface RAS data: we use mesh coords of a surface.
  surface_file = system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE);
  surface = read.fs.surface(surface_file);

  ras = surfaceras.to.ras(mghheader, surface$vertices);
  # transform back
  sras = ras.to.surfaceras(mghheader, ras);
  testthat::expect_equal(surface$vertices, sras);
})


test_that("Transformation from Talairach RAS to RAS and back works", {
  brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
  mgh = read.fs.mgh(brain_image, with_header=TRUE);
  mghheader = mgh$header;

  # get RAS data: we use mesh coords of a surface (in sras) and transform to RAS.
  surface_file = system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE);
  surface = read.fs.surface(surface_file);
  ras = surfaceras.to.ras(mghheader, surface$vertices);

  # transform to Talairach
  talairach = system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE);
  talras = ras.to.talairachras(ras, talairach = talairach);

  # transform back
  orig_ras = talairachras.to.ras(talras, talairach = talairach);
  testthat::expect_equal(ras, orig_ras);
})

