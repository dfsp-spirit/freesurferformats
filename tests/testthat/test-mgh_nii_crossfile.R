# Tests that cross-validate the FreeSurfer MGH/MGZ and the NIfTI reading and writing code using the
# two FreeSurfer-generated reference files for the same brain volume:
#
#   * inst/extdata/brain.mgz        -- part of the package.
#   * extra_test_data/brain.nii     -- NOT part of the package (it would break the 5 MB package size
#                                      limit), it only exists in git checkouts of the repository.
#
# The `extra_test_data` directory is excluded from the built/installed package via `.Rbuildignore`,
# so the tests in this file are skipped whenever it is not available, i.e., on CRAN and for users
# with an installed copy of the package only. Everyone who checked out the repo from git (including
# GitHub Actions) has the data and the tests run there.

testthat::test_that("Reading brain.nii and brain.mgz yields the same volume and geometry", {
  testthat::skip_if_not_installed("oro.nifti")
  brain_nii_file <- find_extra_test_data_file("brain.nii")
  testthat::skip_if(is.null(brain_nii_file), "extra_test_data not available in this environment (a git checkout of the repository is required).")

  brain_mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  mgh <- read.fs.mgh(brain_mgz_file, with_header = TRUE)
  # Disable the oro.nifti reorientation so the data stays in the raw NIfTI file order (first
  # dimension fastest), which is the same order used by the MGH/MGZ format.
  nii <- read.fs.volume.nii(brain_nii_file, with_header = TRUE, reorient = FALSE)

  # Same data type and dimensions.
  testthat::expect_equal(nii$header$dtype, mgh$header$dtype)
  testthat::expect_equal(nii$header$dtype, 0L) # MRI_UCHAR
  testthat::expect_equal(dim(nii$data), c(256, 256, 256, 1))
  testthat::expect_equal(dim(nii$data), dim(mgh$data))

  # Identical voxel data.
  testthat::expect_equal(nii$data, mgh$data)
  testthat::expect_equal(nii$data[100, 100, 100, 1], 77) # mri_info --voxel 99 99 99
  testthat::expect_equal(nii$data[110, 110, 110, 1], 71)
  testthat::expect_equal(nii$data[1, 1, 1, 1], 0)
  testthat::expect_equal(sum(as.numeric(nii$data)), 121035479)

  # Same RAS geometry (voxel sizes, direction cosines, center RAS and the full vox2ras matrix).
  testthat::expect_equal(nii$header$ras_good_flag, 1L)
  testthat::expect_equal(mgh$header$ras_good_flag, 1L)
  testthat::expect_equal(c(nii$header$internal$xsize, nii$header$internal$ysize, nii$header$internal$zsize),
                         c(mgh$header$internal$xsize, mgh$header$internal$ysize, mgh$header$internal$zsize), tolerance = 1e-4)
  testthat::expect_equal(nii$header$internal$Mdc, mgh$header$internal$Mdc, tolerance = 1e-4)
  testthat::expect_equal(c(nii$header$internal$c_r, nii$header$internal$c_a, nii$header$internal$c_s),
                         mgh$header$internal$Pxyz_c, tolerance = 1e-2)
  testthat::expect_equal(mghheader.vox2ras(nii), mghheader.vox2ras(mgh), tolerance = 1e-4)
  testthat::expect_equal(mghheader.crs.orientation(nii), mghheader.crs.orientation(mgh))
  testthat::expect_equal(mghheader.crs.orientation(nii), "LIA")
})


testthat::test_that("Reading brain.nii from its q-form alone yields the same geometry", {
  testthat::skip_if_not_installed("oro.nifti")
  brain_nii_file <- find_extra_test_data_file("brain.nii")
  testthat::skip_if(is.null(brain_nii_file), "extra_test_data not available in this environment (a git checkout of the repository is required).")

  brain_mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  mgh <- read.fs.mgh(brain_mgz_file, with_header = TRUE)

  nifti_img <- oro.nifti::readNIfTI(brain_nii_file, reorient = FALSE)
  nifti_img@sform_code <- 0L # Remove the s-form so that reading falls back to the q-form.
  nii_qform <- read.fs.volume.nii(nifti_img, with_header = TRUE)

  testthat::expect_equal(nii_qform$header$ras_good_flag, 1L)
  # The q-form is a slightly less precise encoding than the s-form, so use a somewhat larger tolerance.
  testthat::expect_equal(mghheader.vox2ras(nii_qform), mghheader.vox2ras(mgh), tolerance = 1e-3)
})


testthat::test_that("A NIfTI header generated from brain.mgz matches the FreeSurfer brain.nii reference", {
  brain_mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  mgh <- read.fs.mgh(brain_mgz_file, with_header = TRUE)
  nii_header <- nii1header.for.mgh(mgh)

  testthat::expect_equal(nii_header$sform_code, 1L)
  testthat::expect_equal(nii_header$qform_code, 1L)
  testthat::expect_equal(nii_header$datatype, 2L) # MRI_UCHAR -> NIFTI UINT8
  testthat::expect_equal(nii_header$bitpix, 8L)

  # The s-form rows and the q-form offset must equal the rows/offset of the FreeSurfer reference
  # file brain.nii (generated with mri_convert from the same brain.mgz).
  testthat::skip_if_not_installed("oro.nifti")
  brain_nii_file <- find_extra_test_data_file("brain.nii")
  testthat::skip_if(is.null(brain_nii_file), "extra_test_data not available in this environment (a git checkout of the repository is required).")

  ref <- oro.nifti::readNIfTI(brain_nii_file, reorient = FALSE)

  testthat::expect_equal(nii_header$srow_x, ref@srow_x, tolerance = 1e-4)
  testthat::expect_equal(nii_header$srow_y, ref@srow_y, tolerance = 1e-4)
  testthat::expect_equal(nii_header$srow_z, ref@srow_z, tolerance = 1e-4)
  testthat::expect_equal(c(nii_header$qoffset_x, nii_header$qoffset_y, nii_header$qoffset_z),
                         c(ref@qoffset_x, ref@qoffset_y, ref@qoffset_z), tolerance = 1e-4)
  testthat::expect_equal(nii_header$pix_dim[1], ref@pixdim[1], tolerance = 1e-4) # qfac
  testthat::expect_equal(nii_header$pix_dim[2:4], ref@pixdim[2:4], tolerance = 1e-4) # voxel sizes
})


testthat::test_that("Writing brain.mgz as NIfTI and reading it back preserves data and geometry", {
  testthat::skip_if_not_installed("oro.nifti")
  brain_mgz_file <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  mgh <- read.fs.mgh(brain_mgz_file, with_header = TRUE)

  nii_file <- tempfile(fileext = ".nii")
  write.fs.volume(nii_file, mgh)

  nii_back <- read.fs.volume.nii(nii_file, with_header = TRUE, reorient = FALSE)
  testthat::expect_equal(nii_back$data, mgh$data)
  testthat::expect_equal(mghheader.vox2ras(nii_back), mghheader.vox2ras(mgh), tolerance = 1e-4)
})
