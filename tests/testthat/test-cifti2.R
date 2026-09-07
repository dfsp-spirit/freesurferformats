# Tests for the CIFTI readers that use files kept in the repository's `extra_test_data`
# directory (dlabel and dtseries), which is excluded from the built package via
# `.Rbuildignore` (the files would exceed the 5 MB CRAN package size limit).
#
# The CIFTI 2 example files used in these tests are provided under the Open Data Commons
# Public Domain Dedication and Licence (PDDL), see http://opendatacommons.org/licenses/pddl/1.0/
# for details. They are the CIFTI 2 example files available at https://www.nitrc.org/projects/cifti/.

test_that("We can read a CIFTI dlabel file as per-vertex label keys", {
  testthat::skip_on_cran() # cannot download / requires a git checkout on CRAN.
  testthat::skip_if_not_installed("cifti")
  dlabel_file <- find_extra_test_data_file(file.path("cifti", "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii"))
  testthat::skip_if(is.null(dlabel_file), message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  # Read the file in client code until the fix for bug #9 in the cifti package
  # (https://github.com/muschellij2/cifti/issues/9) is on CRAN; see test-cifti.R.
  cii <- cifti::read_cifti(dlabel_file)

  parc_lh <- freesurferformats::read.fs.parcellation.cifti(cii, "lh")
  parc_rh <- freesurferformats::read.fs.parcellation.cifti(cii, "rh")
  parc_both <- freesurferformats::read.fs.parcellation.cifti(cii, "both")

  # The Conte69 example file is on the fs_LR 32k mesh (32,492 vertices per hemisphere).
  expect_equal(length(parc_lh), 32492L)
  expect_equal(length(parc_rh), 32492L)
  expect_equal(length(parc_both), 64984L)

  # Returned values are integer label keys (0 = unknown / medial wall label).
  expect_true(is.integer(parc_lh))
  expect_true(all(parc_lh >= 0L, na.rm = TRUE))
  expect_true(sum(parc_lh > 0L) > 1000L)

  # The label table can be returned on demand.
  res <- freesurferformats::read.fs.parcellation.cifti(cii, "lh", with_label_table = TRUE)
  expect_true(is.list(res))
  expect_equal(names(res), c("label_keys", "label_table", "map_name", "brain_structure"))
  expect_true(is.data.frame(res$label_table))
  expect_true(all(c("Key", "Red", "Green", "Blue", "Alpha", "Label") %in% names(res$label_table)))
  expect_true(is.character(res$map_name))
})

test_that("We can read a CIFTI dtseries file as a per-vertex time series matrix", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("cifti")
  dtseries_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"))
  testthat::skip_if(is.null(dtseries_file), message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  cii <- cifti::read_cifti(dtseries_file)

  series_lh <- freesurferformats::read.fs.series.cifti(cii, "lh")
  series_rh <- freesurferformats::read.fs.series.cifti(cii, "rh")
  series_both <- freesurferformats::read.fs.series.cifti(cii, "both")

  expect_true(is.matrix(series_lh))
  expect_equal(nrow(series_lh), 32492L)
  expect_equal(nrow(series_rh), 32492L)
  # The Conte69 example dtseries file has 2 series points.
  expect_equal(ncol(series_lh), 2L)

  expect_true(is.list(series_both))
  expect_equal(names(series_both), c("lh", "rh"))
  expect_equal(dim(series_both$lh), dim(series_lh))
  expect_equal(dim(series_both$rh), dim(series_rh))
})
