# Tests for the CIFTI readers that use files kept in the repository's `extra_test_data`
# directory (dlabel and dtseries), which is excluded from the built package via
# `.Rbuildignore` (the files would exceed the 5 MB CRAN package size limit).
#
# The CIFTI 2 example files used in these tests are provided under the Open Data Commons
# Public Domain Dedication and Licence (PDDL), see http://opendatacommons.org/licenses/pddl/1.0/
# for details. They are the CIFTI 2 example files available at https://www.nitrc.org/projects/cifti/.
#
# The tests in this file pass objects created by the 'cifti' package to the readers. That
# is the path the package documented as a workaround for bug #9 of that package
# (https://github.com/muschellij2/cifti/issues/9); reading is native now, and these tests
# make sure that the workaround still works. The next test file compares the native
# implementation against it.

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

test_that("The native CIFTI reader agrees with the 'cifti' package on the official example files", {
  # The 'cifti' package is an independent implementation (it parses the same files with
  # its own parser and its own data reading code), so comparing both implementations
  # covers the metadata *and* the data order: a transposed reading, a mixup of the
  # hemispheres or a wrong NA pattern would all show up here.
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("cifti")

  dtseries_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"))
  dlabel_file <- find_extra_test_data_file(file.path("cifti", "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii"))
  testthat::skip_if(is.null(dtseries_file) || is.null(dlabel_file),
                    message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  # Morphometry data (one value per vertex, NA for the vertices the file does not contain).
  legacy_dtseries <- cifti::read_cifti(dtseries_file)
  for (structure in c("lh", "rh")) {
    native_morph <- read.fs.morph.cifti(dtseries_file, structure)
    expect_equal(native_morph, read.fs.morph.cifti(legacy_dtseries, structure))
    # The native reader names the columns of the result after the series, the code path
    # through the 'cifti' package does not; compare the values only.
    native_series <- read.fs.series.cifti(dtseries_file, structure)
    expect_equal(unname(native_series), unname(read.fs.series.cifti(legacy_dtseries, structure)))
    expect_equal(colnames(native_series), c("0", "1"))
    # The file has fewer grayordinates than the surface has vertices, and the missing
    # ones (the medial wall) must be NA.
    expect_equal(length(native_morph), 32492L)
    expect_true(sum(is.na(native_morph)) > 1000L)
  }
  expect_equal(read.fs.morph.cifti(dtseries_file, "both"), read.fs.morph.cifti(legacy_dtseries, "both"))

  # Label keys and the label table of a dlabel file.
  legacy_dlabel <- cifti::read_cifti(dlabel_file)
  for (structure in c("lh", "rh")) {
    expect_equal(read.fs.parcellation.cifti(dlabel_file, structure), read.fs.parcellation.cifti(legacy_dlabel, structure))
  }
  native_table <- read.fs.parcellation.cifti(dlabel_file, "lh", with_label_table = TRUE)$label_table
  legacy_table <- read.fs.parcellation.cifti(legacy_dlabel, "lh", with_label_table = TRUE)$label_table
  for (column in c("Key", "Red", "Green", "Blue", "Alpha", "Label")) {
    expect_equal(native_table[[column]], legacy_table[[column]])
  }
})

test_that("Parcellating the official dense time series reproduces the official parcellated file", {
  # This is the strongest check available for the data layout: the official ptseries file
  # was created by Connectome Workbench by averaging the values of the official dtseries
  # over the vertices of each parcel. Doing the same with our reader uses everything at
  # once -- the byte order, the dense index mapping, the surface vertex indices and the
  # parcel vertex lists of both hemispheres -- and the result is a second independent file
  # written by Workbench. A transposed reading of the data, for example, does not pass.
  testthat::skip_on_cran()
  dtseries_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"))
  ptseries_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii"))
  testthat::skip_if(is.null(dtseries_file) || is.null(ptseries_file),
                    message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  parcellated <- read.cifti(ptseries_file)
  expect_equal(dim(parcellated$data), c(2L, 54L)) # 2 series points, 54 parcels
  parcels <- cifti.map.for.dim(parcellated$header, 1L)$parcels
  expect_equal(length(parcels), 54L)
  expect_equal(cifti.parcels(parcellated$header, 1L)$name[1], "MEDIAL.WALL")

  series <- list(lh = read.fs.series.cifti(dtseries_file, "lh"),
                 rh = read.fs.series.cifti(dtseries_file, "rh"))

  parcel_means <- t(sapply(parcels, function(parcel) {
    values <- NULL
    for (structure_short in names(parcel$vertices)) {
      structure_series <- if (identical(structure_short, "CORTEX_LEFT")) {
        series$lh
      } else if (identical(structure_short, "CORTEX_RIGHT")) {
        series$rh
      } else {
        stop(sprintf("Unexpected brain structure '%s' in a parcel.", structure_short))
      }
      vertex_indices <- parcel$vertices[[structure_short]] + 1L # vertex indices are 0-based
      values <- rbind(values, structure_series[vertex_indices, , drop = FALSE])
    }
    return(colMeans(values, na.rm = TRUE))
  }))

  expect_equal(dim(parcel_means), c(54L, 2L))
  expect_equal(unname(parcel_means), unname(t(parcellated$data)), tolerance = 1e-5)
})
