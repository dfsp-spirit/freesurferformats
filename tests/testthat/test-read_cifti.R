# Tests for the CIFTI-2 data reader (R/read_cifti.R) and the native implementations
# of the read.fs.*.cifti() functions.
#
# The small fixtures in inst/extdata/cifti/ are written by Connectome Workbench 2.2.1
# (see dev_tools/generate_cifti_test_data.py, which also documents their exact values).
# The values they contain are highly regular on purpose: the lh metric map m has the
# values 100 * m + vertex_index, the rh metric map m has 200 * m + vertex_index, and
# the volume files have 1000 + 100 * i + 10 * j + k per voxel, so that a mixup of the
# dimensions, of the hemispheres or of the maps is always detectable.

cifti.test.data.file <- function(filename) {
  return(system.file("extdata", "cifti", filename, package = "freesurferformats"))
}

test_that("We can read the data matrix of all shipped CIFTI-2 example files", {
  for (filename in c("tiny.dscalar.nii", "tiny_roi.dscalar.nii", "tiny.dtseries.nii",
                     "tiny.dlabel.nii", "tiny_volume.dscalar.nii", "tiny_volume.dtseries.nii",
                     "tiny_mixed.dscalar.nii", "tiny.dconn.nii", "tiny.pscalar.nii",
                     "tiny.ptseries.nii", "tiny.pconn.nii", "tiny.dpconn.nii", "tiny.pdconn.nii")) {
    cifti_file <- cifti.test.data.file(filename)
    expect_true(file.exists(cifti_file))

    cii <- read.cifti(cifti_file)
    expect_true(inherits(cii, "fs.cifti.data"))
    expect_true(inherits(cii$header, "fs.cifti"))
    expect_equal(as.integer(dim(cii$data)), as.integer(cii$header$matrix$dim_sizes))
    expect_equal(length(dim(cii$data)), 2L)
    expect_true(is.numeric(cii$data))
    expect_false(anyNA(cii$data))
    expect_false(any(is.infinite(cii$data)))

    # The print method must work and must not print all the data.
    expect_output(print(cii), "CIFTI-2 file", fixed = TRUE)
  }
})

test_that("The data reader returns the values in file order (matrix dimension 0 first)", {
  # A transposed reading is the most likely silent error for CIFTI, so check both
  # dimensions against the known values of the fixture. The file has 4 maps (matrix
  # dimension 0) and 22 grayordinates (matrix dimension 1), i.e. 10 lh vertices
  # followed by 12 rh vertices.
  dscalar_file <- cifti.test.data.file("tiny.dscalar.nii")
  cii <- read.cifti(dscalar_file)
  data <- cii$data

  expect_equal(dim(data), c(4L, 22L))

  # One row per map, one column per grayordinate.
  for (map_idx in seq_len(4L)) {
    expect_equal(unname(data[map_idx, 1:10]), 100 * map_idx + (0:9)) # lh: 100 * map + vertex
    expect_equal(unname(data[map_idx, 11:22]), 200 * map_idx + (0:11)) # rh: 200 * map + vertex
  }

  # The same file read with only some columns selected must give the same values.
  selected <- read.cifti(dscalar_file, columns = c(11L, 1L, 22L))
  expect_equal(selected$data, data[, c(11L, 1L, 22L), drop = FALSE])
  expect_equal(dimnames(selected$data)[[2L]], dimnames(data)[[2L]][c(11L, 1L, 22L)])
})

test_that("The reader names the data dimensions with the axis labels", {
  cii <- read.cifti(cifti.test.data.file("tiny.dscalar.nii"))
  expect_equal(dimnames(cii$data)[[1L]], c("lhmap1", "lhmap2", "lhmap3", "lhmap4"))
  expect_equal(dimnames(cii$data)[[2L]],
               c(sprintf("CORTEX_LEFT_%d", 0:9), sprintf("CORTEX_RIGHT_%d", 0:11)))

  # A series dimension is labelled with the series values (times).
  series_cii <- read.cifti(cifti.test.data.file("tiny.dtseries.nii"))
  expect_equal(dimnames(series_cii$data)[[1L]], c("0", "2.5", "5", "7.5"))

  # A parcellated dimension is labelled with the parcel names.
  parcel_cii <- read.cifti(cifti.test.data.file("tiny.ptseries.nii"))
  expect_equal(dimnames(parcel_cii$data)[[2L]], c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
})

test_that("The row and column selections of read.cifti() are validated", {
  dscalar_file <- cifti.test.data.file("tiny.dscalar.nii")
  data <- read.cifti(dscalar_file)$data

  expect_equal(read.cifti(dscalar_file, rows = c(2L, 4L))$data, data[c(2L, 4L), , drop = FALSE])
  expect_equal(read.cifti(dscalar_file, rows = 3L, columns = 2L)$data, data[3L, 2L, drop = FALSE])
  expect_equal(dim(read.cifti(dscalar_file, rows = 1L)$data), c(1L, 22L))
  expect_equal(dim(read.cifti(dscalar_file, columns = 5L)$data), c(4L, 1L))

  # Indices are 1-based and must be in range.
  expect_error(read.cifti(dscalar_file, rows = 0L), "in range 1 to 4")
  expect_error(read.cifti(dscalar_file, rows = 5L), "in range 1 to 4")
  expect_error(read.cifti(dscalar_file, columns = 23L), "in range 1 to 22")
  expect_error(read.cifti(dscalar_file, columns = NA_integer_), "in range 1 to 22")
  expect_error(read.cifti(dscalar_file, rows = 1.5), "in range 1 to 4")
  expect_error(read.cifti(dscalar_file, rows = "1"), "must be an integer vector")
})

test_that("The allocation guard of read.cifti() points to the column selection", {
  dscalar_file <- cifti.test.data.file("tiny.dscalar.nii")
  old_options <- options(freesurferformats.max_alloc_bytes = 100)
  on.exit(options(old_options), add = TRUE)

  # 4 x 22 float32 values are 352 bytes, more than the 100 bytes we allow here.
  expect_error(read.cifti(dscalar_file), "exceeds the safety limit")
  expect_error(read.cifti(dscalar_file), "columns")

  # Two columns are 32 bytes and can be read.
  cii <- read.cifti(dscalar_file, columns = c(1L, 2L))
  expect_equal(dim(cii$data), c(4L, 2L))
  expect_equal(unname(cii$data[, 1L]), c(100, 200, 300, 400))
})

test_that("We can extract the data of a single surface structure", {
  cii <- read.cifti(cifti.test.data.file("tiny.dscalar.nii"))

  lh <- cifti.structure.data(cii, "lh")
  expect_equal(names(lh), c("structure", "structure_short", "model_type", "surface", "volume"))
  expect_equal(lh$structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(lh$structure_short, "CORTEX_LEFT")
  expect_equal(lh$model_type, "SURFACE")
  expect_null(lh$volume)
  expect_equal(dim(lh$surface), c(10L, 4L))
  expect_equal(lh$surface[, 1L], 100:109)
  expect_equal(lh$surface[, 4L], 400:409)
  expect_equal(dimnames(lh$surface)[[2L]], c("lhmap1", "lhmap2", "lhmap3", "lhmap4"))

  rh <- cifti.structure.data(cii, "CIFTI_STRUCTURE_CORTEX_RIGHT")
  expect_equal(dim(rh$surface), c(12L, 4L))
  expect_equal(rh$surface[, 2L], 400:411)

  # All spellings of the structure name are accepted.
  for (structure in list("lh", "left", "CORTEX_LEFT", "CortexLeft", "CIFTI_STRUCTURE_CORTEX_LEFT")) {
    expect_equal(cifti.structure.data(cii, structure)$surface, lh$surface)
  }

  # Without a structure, the data of all structures are returned.
  all_structures <- cifti.structure.data(cii)
  expect_equal(names(all_structures), c("CORTEX_LEFT", "CORTEX_RIGHT"))
  expect_equal(all_structures$CORTEX_LEFT$surface, lh$surface)

  expect_error(cifti.structure.data(cii, "no_such_structure"), "contains no brain structure named")
})

test_that("Vertices that the file does not contain are reported as NA", {
  # The roi file only keeps the even vertices of each surface.
  cii <- read.cifti(cifti.test.data.file("tiny_roi.dscalar.nii"))
  grayordinates <- cifti.grayordinates(cii$header, 1L)
  expect_equal(grayordinates$vertex_index[grayordinates$structure_short == "CORTEX_LEFT"], c(0L, 2L, 4L, 6L, 8L))
  expect_equal(dim(cii$data), c(4L, 11L))

  lh <- cifti.structure.data(cii, "lh")$surface
  # The result has the size of the complete surface, with NA for the missing vertices.
  expect_equal(dim(lh), c(10L, 4L))
  expect_equal(lh[, 1L], c(100, NA, 102, NA, 104, NA, 106, NA, 108, NA))
  expect_equal(sum(is.na(lh[, 1L])), 5L)
})

test_that("We can extract the data of volume structures", {
  cii <- read.cifti(cifti.test.data.file("tiny_volume.dscalar.nii"))
  expect_equal(dim(cii$data), c(1L, 63L)) # 4x4x4 voxels, one of them is not part of the file

  cerebellum <- cifti.structure.data(cii, "CEREBELLUM")
  expect_equal(cerebellum$model_type, "VOXELS")
  expect_null(cerebellum$surface)

  volume <- cerebellum$volume
  expect_equal(names(volume), c("values", "voxel_indices_ijk", "dimensions", "meter_exponent", "transformation_matrix"))
  expect_equal(volume$dimensions, c(4L, 4L, 4L))
  expect_equal(nrow(volume$values), nrow(volume$voxel_indices_ijk))
  expect_equal(ncol(volume$voxel_indices_ijk), 3L)
  expect_equal(nrow(volume$voxel_indices_ijk), cifti.structures(cii$header, 1L)$index_count[3])
  expect_equal(dim(volume$transformation_matrix), c(4L, 4L))
  expect_true(is.numeric(volume$meter_exponent))

  # The voxel values follow the pattern 1000 + 100 * i + 10 * j + k of the fixture.
  for (row_idx in seq_len(nrow(volume$values))) {
    ijk <- unname(volume$voxel_indices_ijk[row_idx, ])
    expect_equal(unname(volume$values[row_idx, 1L]), 1000 + 100 * ijk[1] + 10 * ijk[2] + ijk[3])
  }

  cortex_left <- cifti.structure.data(cii, "lh")
  expect_null(cortex_left$surface)
  expect_equal(nrow(cortex_left$volume$voxel_indices_ijk), 32L)
})

test_that("We can extract the data of a structure that has a surface and a volume part", {
  # This file contains the lh surface with a reduced mapping (only the even
  # vertices), the rh surface and the volume structures of the other fixtures.
  cii <- read.cifti(cifti.test.data.file("tiny_mixed.dscalar.nii"))
  lh <- cifti.structure.data(cii, "lh")
  expect_equal(lh$model_type, "SURFACE_AND_VOXELS")
  expect_equal(dim(lh$surface), c(10L, 4L))
  expect_equal(lh$surface[, 1L], c(100, NA, 102, NA, 104, NA, 106, NA, 108, NA))
  expect_equal(nrow(lh$volume$voxel_indices_ijk), 32L)
  expect_equal(dim(lh$volume$values), c(32L, 4L))
})

test_that("Extracting the data of a connectome file requires an explicit dimension", {
  cii <- read.cifti(cifti.test.data.file("tiny.dconn.nii"))
  expect_equal(dim(cii$data), c(22L, 22L))

  # Brain models are in both dimensions, so the structure cannot be selected automatically.
  expect_error(cifti.structure.data(cii, "lh"), "has brain models in both matrix dimensions")

  # With an explicit dimension it works, and the other matrix dimension becomes the
  # columns of the result.
  lh_dim1 <- cifti.structure.data(cii, "lh", dim = 1L)
  expect_equal(dim(lh_dim1$surface), c(10L, 22L))
  expect_equal(lh_dim1$surface, matrix(1, nrow = 10L, ncol = 22L,
                                       dimnames = list(NULL, dimnames(cii$data)[[1L]])))
  lh_dim0 <- cifti.structure.data(cii, "lh", dim = 0L)
  expect_equal(dim(lh_dim0$surface), c(10L, 22L))

  # A dense connectome with parcels in the second dimension only has brain models
  # in dimension 0, so the dimensions can be told apart by their size.
  dpconn <- read.cifti(cifti.test.data.file("tiny.dpconn.nii"))
  expect_equal(dim(dpconn$data), c(22L, 3L))
  expect_equal(dim(cifti.structure.data(dpconn, "lh", dim = 0L)$surface), c(10L, 3L))
  expect_error(cifti.structure.data(dpconn, "lh", dim = 1L), "not 'CIFTI_INDEX_TYPE_BRAIN_MODELS'")

  # cifti.structure.data() accepts a file path or a header as well as a data object.
  expect_equal(cifti.structure.data(cifti.test.data.file("tiny.dscalar.nii"), "lh")$surface,
               cifti.structure.data(read.cifti.header(cifti.test.data.file("tiny.dscalar.nii")), "lh")$surface)
  expect_error(cifti.structure.data(cifti.test.data.file("tiny.dscalar.nii"), "lh", dim = 0L),
               "not 'CIFTI_INDEX_TYPE_BRAIN_MODELS'")
  expect_error(cifti.structure.data(list()), "must be an fs.cifti.data object")
  expect_error(cifti.structure.data("no_such_file.nii"), "does not exist")
})

test_that("We can get the brainordinate table of a CIFTI-2 file", {
  cii <- read.cifti.header(cifti.test.data.file("tiny.dscalar.nii"))
  grayordinates <- cifti.grayordinates(cii, 1L)
  expect_equal(names(grayordinates),
               c("index", "structure", "structure_short", "model_type", "vertex_index", "i", "j", "k"))
  expect_equal(nrow(grayordinates), 22L)
  expect_equal(grayordinates$index, 0:21)
  expect_equal(grayordinates$structure_short, c(rep("CORTEX_LEFT", 10L), rep("CORTEX_RIGHT", 12L)))
  expect_equal(grayordinates$model_type, rep("SURFACE", 22L))
  expect_equal(grayordinates$vertex_index, c(0:9, 0:11))
  expect_true(all(is.na(grayordinates$i)))
  expect_true(all(is.na(grayordinates$j)))
  expect_true(all(is.na(grayordinates$k)))

  # Volume models report the voxel indices instead of the vertex index.
  volume_tab <- cifti.grayordinates(read.cifti.header(cifti.test.data.file("tiny_volume.dscalar.nii")), 1L)
  expect_true(all(is.na(volume_tab$vertex_index)))
  expect_equal(volume_tab$index, 0:62)
  expect_equal(volume_tab$i[1], 0L)
  expect_equal(volume_tab$i[2], 1L) # the first voxel index varies fastest
  expect_equal(nrow(volume_tab), sum(cifti.structures(read.cifti.header(cifti.test.data.file("tiny_volume.dscalar.nii")), 1L)$index_count))

  # The first brain model of the file covers the first entries.
  expect_equal(cifti.grayordinates(cii, 1L)$index[1], cifti.structures(cii, 1L)$index_offset[1])

  # A parcellated dimension has no brainordinates.
  parcel_cii <- read.cifti.header(cifti.test.data.file("tiny.ptseries.nii"))
  expect_error(cifti.grayordinates(parcel_cii, 1L), "no brainordinates")
})

test_that("We can get the axis labels of all CIFTI-2 mapping types", {
  dscalar <- read.cifti.header(cifti.test.data.file("tiny.dscalar.nii"))
  expect_equal(cifti.dim.labels(dscalar, 0L), c("lhmap1", "lhmap2", "lhmap3", "lhmap4"))
  expect_length(cifti.dim.labels(dscalar, 1L), 22L)

  labels <- read.cifti.header(cifti.test.data.file("tiny.dlabel.nii"))
  # Connectome Workbench names label maps "#<number>".
  expect_equal(cifti.dim.labels(labels, 0L), c("#1"))

  series <- read.cifti.header(cifti.test.data.file("tiny.dtseries.nii"))
  expect_equal(cifti.dim.labels(series, 0L), c("0", "2.5", "5", "7.5"))

  parcels <- read.cifti.header(cifti.test.data.file("tiny.pconn.nii"))
  expect_equal(cifti.dim.labels(parcels, 0L), c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(cifti.dim.labels(parcels, 1L), c("PARCEL_A", "PARCEL_B", "PARCEL_C"))

  # Maps without a name get a generic label.
  volume_scalars <- read.cifti.header(cifti.test.data.file("tiny_volume.dscalar.nii"))
  expect_equal(cifti.dim.labels(volume_scalars, 0L), "map_0")

  expect_error(cifti.dim.labels(dscalar, 2L), "in range 0 to 1")
})

test_that("We can read morphometry data and time series directly from the file", {
  dscalar_file <- cifti.test.data.file("tiny.dscalar.nii")
  morph_lh <- read.fs.morph.cifti(dscalar_file, "lh")
  expect_equal(morph_lh, 100:109)
  expect_equal(read.fs.morph.cifti(dscalar_file, "lh", 2L), 200:209)
  expect_equal(read.fs.morph.cifti(dscalar_file, "rh"), 200:211)
  expect_equal(read.fs.morph.cifti(dscalar_file, "both"), c(100:109, 200:211))
  expect_equal(read.fs.morph.cifti(dscalar_file, 1L), 100:109) # structure index

  # An fs.cifti or fs.cifti.data object is accepted instead of the path.
  cii <- read.cifti(dscalar_file)
  expect_equal(read.fs.morph.cifti(cii, "lh"), morph_lh)
  expect_equal(read.fs.morph.cifti(cii$header, "lh"), morph_lh)

  # The roi file has NA for the vertices that are not part of it.
  roi_file <- cifti.test.data.file("tiny_roi.dscalar.nii")
  expect_equal(read.fs.morph.cifti(roi_file, "lh"), c(100, NA, 102, NA, 104, NA, 106, NA, 108, NA))

  # Time series are returned as one row per vertex, one column per series point.
  series_lh <- read.fs.series.cifti(cifti.test.data.file("tiny.dtseries.nii"), "lh")
  expect_equal(dim(series_lh), c(10L, 4L))
  expect_equal(series_lh[, 1L], 100:109)
  expect_equal(series_lh[, 4L], 400:409)
  series_both <- read.fs.series.cifti(cifti.test.data.file("tiny.dtseries.nii"), "both")
  expect_equal(names(series_both), c("lh", "rh"))
  expect_equal(dim(series_both$rh), c(12L, 4L))

  # Volume structures cannot be returned as per-vertex data, and the error says why.
  volume_file <- cifti.test.data.file("tiny_volume.dscalar.nii")
  expect_error(read.fs.morph.cifti(volume_file, "CEREBELLUM"), "has no surface part")
  expect_error(read.fs.morph.cifti(volume_file, "no_such_structure"), "contains no brain structure named")
  expect_error(read.fs.morph.cifti(dscalar_file, 5L), "there is no structure with index 5")
})

test_that("We can read label data directly from the file", {
  dlabel_file <- cifti.test.data.file("tiny.dlabel.nii")
  parc_lh <- read.fs.parcellation.cifti(dlabel_file, "lh")
  expect_true(is.integer(parc_lh))
  expect_equal(parc_lh, c(0L, 1L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L))
  expect_equal(read.fs.parcellation.cifti(dlabel_file, "rh"),
               c(3L, 3L, 2L, 2L, 1L, 1L, 0L, 0L, 1L, 2L, 3L, 3L))
  expect_equal(read.fs.parcellation.cifti(dlabel_file, "both"), c(parc_lh, c(3L, 3L, 2L, 2L, 1L, 1L, 0L, 0L, 1L, 2L, 3L, 3L)))

  with_table <- read.fs.parcellation.cifti(dlabel_file, "lh", with_label_table = TRUE)
  expect_equal(names(with_table), c("label_keys", "label_table", "map_name", "brain_structure"))
  expect_equal(with_table$label_keys, parc_lh)
  expect_equal(with_table$brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(names(with_table$label_table), c("Key", "Red", "Green", "Blue", "Alpha", "Label"))
  expect_equal(with_table$label_table$Key, 0:3)
  expect_equal(with_table$label_table$Label, c("???", "PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(with_table$label_table$Red, c(1, 0, 0, 0))
  expect_equal(with_table$label_table$Blue, c(1, 1, 1, 1))

  both_with_table <- read.fs.parcellation.cifti(dlabel_file, "both", with_label_table = TRUE)
  expect_equal(names(both_with_table), c("lh", "rh"))
  expect_equal(both_with_table$rh$label_keys, read.fs.parcellation.cifti(dlabel_file, "rh"))
})

test_that("The CIFTI-2 data reader rejects invalid input", {
  dscalar_file <- cifti.test.data.file("tiny.dscalar.nii")
  expect_error(read.cifti("no_such_file.nii"), "does not exist")
  expect_error(read.cifti(dscalar_file, rows = NULL, columns = NULL), NA)

  # Gzipped CIFTI files do not exist, the format forbids compression.
  gz_file <- file.path(tempdir(), "cifti_test_gz.dscalar.nii.gz")
  writeBin(as.raw(1:100), gz_file)
  expect_error(read.cifti(gz_file), "must not be compressed")

  # A NIFTI-2 file without the CIFTI XML extension is not a CIFTI file.
  nifti_file <- file.path(tempdir(), "cifti_test_plain.nii")
  write.nifti2(array(1:27, dim = c(3L, 3L, 3L)), filepath = nifti_file)
  expect_error(read.cifti(nifti_file), "no NIFTI v2 header extension with code 32")
})
