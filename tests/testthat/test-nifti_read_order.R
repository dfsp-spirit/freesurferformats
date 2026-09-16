# Regression tests for the NIfTI volume reader (`read.fs.volume.nii`).
#
# The reader must return the voxel data in the raw NIfTI file storage order (first dimension fastest), because
# the geometry in the returned header (`mghheader.vox2ras` / `header$vox2ras_matrix`) describes exactly that
# order. oro.nifti::readNIfTI() can reorient the data array on read, but does not update the `sform` header
# fields, so the data array and the geometry derived from those fields would disagree (silently, since the
# voxel values are merely permuted). See the note in the documentation of `read.fs.volume.nii`.

# Write a copy of a NIfTI v1 file with the sform replaced by the given 4x4 matrix, the voxel sizes (pixdim)
# updated to match it, and the qform removed. This allows testing oblique/rotated geometry without shipping
# extra test data. Byte offsets are 0-based and refer to the NIfTI-1 header layout, see
# https://nifti.nimh.nih.gov/nifti-1/.
write.nifti.with.sform <- function(src_file, sform, dest_file) {
  bytes <- readBin(src_file, "raw", n = file.info(src_file)$size)
  put.f32 <- function(b, off, vals) {
    r <- writeBin(as.numeric(vals), raw(), size = 4, endian = "little")
    b[off + seq_along(r)] <- r
    b
  }
  put.i16 <- function(b, off, vals) {
    v <- as.numeric(vals)
    v[v < 0] <- v[v < 0] + 2^16
    r <- as.raw(c(rbind(v %% 256, (v %/% 256) %% 256)))
    b[off + seq_along(r)] <- r
    b
  }
  bytes <- put.i16(bytes, 254L, 2L) # sform_code: aligned
  bytes <- put.i16(bytes, 252L, 0L) # qform_code: unknown
  bytes <- put.f32(bytes, 76L, c(1, sqrt(colSums(sform[1:3, 1:3]^2)), 1, 1, 1)) # pixdim[8], pixdim[1] is qfac
  bytes <- put.f32(bytes, 280L, sform[1, ]) # srow_x
  bytes <- put.f32(bytes, 296L, sform[2, ]) # srow_y
  bytes <- put.f32(bytes, 312L, sform[3, ]) # srow_z
  writeBin(bytes, dest_file)
  dest_file
}


test_that("The NIfTI reader returns the raw NIfTI file storage order", {
  testthat::skip_if_not_installed("oro.nifti")
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)

  # This file has a permuted sform (the axes are not aligned with the RAS axes), so it is affected by
  # the reorientation issue.
  order_default <- read.fs.volume.nii(nii_file, with_header = TRUE)
  order_explicit <- read.fs.volume.nii(nii_file, with_header = TRUE, reorient = FALSE)
  expect_equal(order_default$data, order_explicit$data)

  # The data must be identical to the raw data as read by our own low-level NIfTI reader, which never reorients.
  expect_equal(drop(order_default$data), read.nifti1.data(nii_file, drop_empty_dims = FALSE))
})


test_that("The NIfTI reader returns a vox2ras matrix that matches the data ordering", {
  testthat::skip_if_not_installed("oro.nifti")
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  fs_volume <- read.fs.volume.nii(nii_file, with_header = TRUE)
  order_default_voxel <- fs_volume$data[1, 1, 1, 1]

  expect_true(mghheader.is.ras.valid(fs_volume$header))
  expect_true("vox2ras_matrix" %in% names(fs_volume$header))
  expect_equal(fs_volume$header$vox2ras_matrix, mghheader.vox2ras(fs_volume$header))

  # The affine must be the sform of the file.
  nii_header <- read.nifti1.header(nii_file)
  sform <- rbind(nii_header$srow_x, nii_header$srow_y, nii_header$srow_z, c(0, 0, 0, 1))
  expect_equal(unname(fs_volume$header$vox2ras_matrix), unname(sform), tolerance = 1e-5)

  # The data must be in the order described by that affine: voxel (0,0,0) of the file is at the sform origin.
  expect_equal(order_default_voxel, 1)
})


test_that("Oblique volumes can be read with the default arguments", {
  testthat::skip_if_not_installed("oro.nifti")
  nii_file <- system.file("extdata", "tiny.nii", package = "freesurferformats", mustWork = TRUE)

  theta <- 23 * pi / 180
  sform <- rbind(
    c(cos(theta), -sin(theta), 0, 10),
    c(sin(theta), cos(theta), 0, -20),
    c(0, 0, 1.4, 30),
    c(0, 0, 0, 1)
  )
  oblique_file <- write.nifti.with.sform(nii_file, sform, tempfile(fileext = ".nii"))

  # Reading an oblique volume used to fail with an error from oro.nifti ("Transformation is not simple").
  fs_volume <- read.fs.volume.nii(oblique_file, with_header = TRUE)
  expect_true(is.fs.volume(fs_volume))

  # The geometry is stored as voxel sizes and direction cosines in the MGH header, so it is not exact for
  # rotated volumes, but the error is far below any relevant precision.
  nii_header <- read.nifti1.header(oblique_file)
  oblique_sform <- rbind(nii_header$srow_x, nii_header$srow_y, nii_header$srow_z, c(0, 0, 0, 1))
  expect_equal(unname(mghheader.vox2ras(fs_volume$header)), unname(oblique_sform), tolerance = 1e-6)

  # The voxel data are untouched by the geometry test above.
  expect_equal(fs_volume$data, read.fs.volume.nii(nii_file, with_header = TRUE)$data)
})


test_that("Passing a reoriented nifti instance warns", {
  testthat::skip_if_not_installed("oro.nifti")
  nii_file <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  nifti_img <- oro.nifti::readNIfTI(nii_file, reorient = TRUE)
  expect_warning(read.fs.volume.nii(nifti_img, with_header = TRUE), "reoriented")
})


test_that("Reading brain.nii with the default arguments yields the geometry and data of the brain.mgz reference", {
  testthat::skip_if_not_installed("oro.nifti")
  brain_nii_file <- find_extra_test_data_file("brain.nii")
  testthat::skip_if(is.null(brain_nii_file), "extra_test_data not available in this environment (a git checkout of the repository is required).")
  brain_mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)

  mgh <- read.fs.mgh(brain_mgz_file, with_header = TRUE)
  nii <- read.fs.volume.nii(brain_nii_file, with_header = TRUE) # default arguments on purpose

  expect_equal(nii$data, mgh$data)
  expect_equal(nii$header$vox2ras_matrix, mghheader.vox2ras(mgh), tolerance = 1e-4)
  expect_equal(mghheader.vox2ras(nii), mghheader.vox2ras(mgh), tolerance = 1e-4)
})
