# Tests for the CIFTI-2 XML reader (read.cifti.header) and its accessor functions.
#
# Most tests use the small fixture files in inst/extdata/cifti, which were created
# with Connectome Workbench (see dev_tools/generate_cifti_test_data.py). The error
# paths are tested with synthetic CIFTI-2 files written with write.nifti2() and a
# hand-made XML extension, which allows testing each single inconsistency.

# --- helpers -----------------------------------------------------------------

# Create a CIFTI-2 file with the given XML metadata and matrix dimensions, in a temp file.
cifti.test.file <- function(xml, dim_sizes = c(1L, 1L), filepath = NULL) {
  if (is.null(filepath)) {
    filepath <- tempfile(fileext = ".nii")
  }
  num_values <- prod(dim_sizes)
  niidata <- as.double(seq_len(num_values))
  niiheader <- ni2header.for.data(niidata)
  niiheader$dim <- c(6L, 1L, 1L, 1L, 1L, as.integer(dim_sizes), 1L)
  niiheader$intent_code <- 3006L
  write.nifti2(filepath, niidata, niiheader, extensions = list(nifti2.extension(32L, xml)))
  return(filepath)
}

# Build the XML document from the given MatrixIndicesMap elements.
cifti.test.xml <- function(maps_xml, version = "2", metadata = "", root = "CIFTI") {
  metadata_xml <- if (identical(metadata, "")) "" else paste0("<MetaData>", metadata, "</MetaData>")
  return(paste0(
    '<?xml version="1.0" encoding="UTF-8"?><', root, ' Version="', version, '"><Matrix>',
    metadata_xml, maps_xml, "</Matrix></", root, ">"
  ))
}

# XML for a surface brain model. If 'vertices' is NULL, no VertexIndices element is written.
cifti.test.bm.surface <- function(offset, count, structure = "CIFTI_STRUCTURE_CORTEX_LEFT", num_verts = count, vertices = NULL) {
  vertices_xml <- if (is.null(vertices)) {
    ""
  } else {
    paste0("<VertexIndices>", paste(vertices, collapse = " "), "</VertexIndices>")
  }
  return(paste0(
    '<BrainModel IndexOffset="', offset, '" IndexCount="', count, '" BrainStructure="', structure,
    '" ModelType="CIFTI_MODEL_TYPE_SURFACE" SurfaceNumberOfVertices="', num_verts, '">', vertices_xml, "</BrainModel>"
  ))
}

# XML for a volume brain model. 'voxels' is a flat vector of IJK triplets.
cifti.test.bm.volume <- function(offset, count, structure = "CIFTI_STRUCTURE_CEREBELLUM", voxels) {
  return(paste0(
    '<BrainModel IndexOffset="', offset, '" IndexCount="', count, '" BrainStructure="', structure,
    '" ModelType="CIFTI_MODEL_TYPE_VOXELS"><VoxelIndicesIJK>', paste(voxels, collapse = " "), "</VoxelIndicesIJK></BrainModel>"
  ))
}

# XML for a Volume element with the given dimensions.
cifti.test.volume <- function(dims = c(2L, 2L, 2L), meter_exponent = -3L) {
  matrix_values <- c(1, 0, 0, -1, 0, 1, 0, -1, 0, 0, 1, -1, 0, 0, 0, 1)
  return(paste0(
    '<Volume VolumeDimensions="', paste(dims, collapse = ","), '"><TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="',
    meter_exponent, '">', paste(matrix_values, collapse = " "), "</TransformationMatrixVoxelIndicesIJKtoXYZ></Volume>"
  ))
}

cifti.test.map.scalars <- function() {
  return('<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"/>')
}

cifti.test.map.brain.models <- function(models_xml, volumes_xml = "", dim = "1") {
  return(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="', dim, '" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">',
    volumes_xml, models_xml, "</MatrixIndicesMap>"
  ))
}

# The paths of the shipped fixtures.
cifti.fixture <- function(name) {
  return(system.file("extdata", "cifti", name, package = "freesurferformats"))
}


# --- the shipped fixtures ----------------------------------------------------

test_that("all shipped CIFTI-2 fixtures can be read", {
  fixture_dir <- system.file("extdata", "cifti", package = "freesurferformats")
  fixture_files <- list.files(fixture_dir, pattern = "[.]nii$", full.names = TRUE)
  expect_equal(length(fixture_files), 13L)
  for (fixture_file in fixture_files) {
    cii <- read.cifti.header(fixture_file)
    expect_true(inherits(cii, "fs.cifti"))
    expect_equal(cii$version, "2")
    expect_equal(cii$filepath, fixture_file)
    expect_equal(length(cii$matrix$dim_sizes), 2L)
    # Every matrix dimension is described exactly once (connectome files have a
    # single mapping that applies to both dimensions).
    all_dims <- sort(unlist(lapply(cii$matrix$indices_maps, function(m) m$dims)))
    expect_equal(all_dims, c(0L, 1L))
    expect_true(all(cii$matrix$dim_sizes > 0L))
  }
})


test_that("the dense scalar fixture has 2 surface brain models", {
  cii <- read.cifti.header(cifti.fixture("tiny.dscalar.nii"))
  expect_equal(cii$matrix$dim_sizes, c(4L, 22L))
  expect_equal(length(cii$matrix$indices_maps), 2L)

  expect_equal(cii$matrix$indices_maps[[1]]$dims, 0L)
  expect_equal(cii$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_SCALARS")
  expect_equal(length(cii$matrix$indices_maps[[1]]$named_maps), 4L)
  expect_equal(cii$matrix$indices_maps[[1]]$named_maps[[1]]$name, "lhmap1")

  map <- cii$matrix$indices_maps[[2]]
  expect_equal(map$dims, 1L)
  expect_equal(map$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_equal(length(map$brain_models), 2L)
  expect_equal(map$brain_models[[1]]$brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(map$brain_models[[1]]$model_type, "CIFTI_MODEL_TYPE_SURFACE")
  expect_equal(map$brain_models[[1]]$surface_number_of_vertices, 10L)
  expect_equal(map$brain_models[[1]]$vertex_indices, 0:9)
  expect_equal(map$brain_models[[2]]$vertex_indices, 0:11)
  expect_null(map$brain_models[[1]]$voxel_indices_ijk)

  structures <- cifti.structures(cii, dim = 1L)
  expect_true(is.data.frame(structures))
  expect_equal(colnames(structures), c("structure", "structure_short", "model_type", "index_offset", "index_count", "surface_number_of_vertices"))
  expect_equal(structures$index_offset, c(0L, 10L))
  expect_equal(structures$index_count, c(10L, 12L))
  expect_equal(structures$structure_short, c("CORTEX_LEFT", "CORTEX_RIGHT"))
})


test_that("the reduced mapping fixture has fewer indices than surface vertices", {
  cii <- read.cifti.header(cifti.fixture("tiny_roi.dscalar.nii"))
  expect_equal(cii$matrix$dim_sizes, c(4L, 11L))
  structures <- cifti.structures(cii, dim = 1L)
  expect_equal(structures$index_count, c(5L, 6L))
  expect_equal(structures$surface_number_of_vertices, c(10L, 12L))
  # The vertex indices are non-contiguous because a ROI was used to select every other vertex.
  expect_equal(cii$matrix$indices_maps[[2]]$brain_models[[1]]$vertex_indices, c(0L, 2L, 4L, 6L, 8L))
  expect_equal(cii$matrix$indices_maps[[2]]$brain_models[[2]]$vertex_indices, c(0L, 2L, 4L, 6L, 8L, 10L))
})


test_that("the dense timeseries fixture has series information", {
  cii <- read.cifti.header(cifti.fixture("tiny.dtseries.nii"))
  expect_equal(cii$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_SERIES")
  series <- cifti.series.info(cii, dim = 0L)
  expect_equal(series$number_of_series_points, 4L)
  expect_equal(series$start, 0)
  expect_equal(series$step, 2.5)
  expect_equal(series$exponent, 0L)
  expect_equal(series$unit, "SECOND")
  # A brain models dimension has no series info.
  expect_null(cifti.series.info(cii, dim = 1L))
})


test_that("the dense label fixture has label tables", {
  cii <- read.cifti.header(cifti.fixture("tiny.dlabel.nii"))
  expect_equal(cii$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_LABELS")
  expect_equal(length(cii$matrix$indices_maps[[1]]$named_maps), 1L)

  label_table <- cifti.label.table(cii, dim = 0L, map = 1L)
  expect_true(is.data.frame(label_table))
  expect_equal(colnames(label_table), c("key", "red", "green", "blue", "alpha", "label", "x", "y", "z"))
  expect_equal(label_table$key, 0:3)
  expect_equal(label_table$label, c("???", "PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(label_table$red[2L], 0)
  expect_equal(label_table$green[2L], 0.5)
  expect_equal(label_table$blue[2L], 1)
  expect_true(is.na(label_table$x[1L]))

  # The brain models dimension of that file has no named maps, so there is no label table.
  expect_null(cifti.label.table(cii, dim = 1L, map = 1L))
  # There is only one map in dimension 0.
  expect_error(cifti.label.table(cii, dim = 0L, map = 2L), "range 1 to 1")
})


test_that("a single mapping can apply to both dimensions (connectome files)", {
  dconn <- read.cifti.header(cifti.fixture("tiny.dconn.nii"))
  expect_equal(length(dconn$matrix$indices_maps), 1L)
  expect_equal(dconn$matrix$indices_maps[[1]]$dims, c(0L, 1L))
  expect_equal(dconn$matrix$indices_maps[[1]]$applies_to, "0,1")
  expect_equal(dconn$matrix$indices_maps[[1]]$size, c(22L, 22L))
  expect_equal(dconn$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  # The same data must be returned for both dimensions.
  expect_equal(cifti.structures(dconn, dim = 0L), cifti.structures(dconn, dim = 1L))

  pconn <- read.cifti.header(cifti.fixture("tiny.pconn.nii"))
  expect_equal(length(pconn$matrix$indices_maps), 1L)
  expect_equal(pconn$matrix$indices_maps[[1]]$dims, c(0L, 1L))
  expect_equal(pconn$matrix$indices_maps[[1]]$size, c(3L, 3L))
  expect_equal(pconn$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_equal(nrow(cifti.parcels(pconn, dim = 1L)), 3L)
  expect_equal(cifti.parcels(pconn, dim = 0L), cifti.parcels(pconn, dim = 1L))
})


test_that("the parcellated fixtures have parcels with vertex lists", {
  cii <- read.cifti.header(cifti.fixture("tiny.ptseries.nii"))
  expect_equal(cifti.series.info(cii, dim = 0L)$number_of_series_points, 4L)
  expect_equal(cii$matrix$indices_maps[[2]]$type, "CIFTI_INDEX_TYPE_PARCELS")

  parcels <- cifti.parcels(cii, dim = 1L)
  expect_equal(colnames(parcels), c("index", "name", "num_vertices", "num_voxels"))
  expect_equal(parcels$index, 0:2)
  expect_equal(parcels$name, c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(parcels$num_vertices, c(6L, 6L, 7L))
  expect_equal(parcels$num_voxels, c(0L, 0L, 0L))

  # The vertex lists are named by the canonical structure name, the indices are 0-based.
  parcel_a <- cii$matrix$indices_maps[[2]]$parcels[[1]]
  expect_equal(names(parcel_a$vertices), c("CORTEX_LEFT", "CORTEX_RIGHT"))
  expect_equal(parcel_a$vertices$CORTEX_LEFT, c(1L, 2L, 7L))
  expect_equal(parcel_a$vertices$CORTEX_RIGHT, c(4L, 5L, 8L))
  expect_equal(parcel_a$index, 0L)

  # Surfaces declared in the parcels mapping.
  surfaces <- cii$matrix$indices_maps[[2]]$surfaces
  expect_equal(length(surfaces), 2L)
  expect_equal(surfaces[[1]]$brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(surfaces[[1]]$surface_number_of_vertices, 10L)
  expect_equal(cifti.surface.vertex.counts(cii$matrix$indices_maps[[2]]), c(CORTEX_LEFT = 10L, CORTEX_RIGHT = 12L))

  # Asking for parcels of a brain models dimension is an error, asking for
  # structures of a parcels dimension is an error as well.
  expect_error(cifti.parcels(cii, dim = 0L), "CIFTI_INDEX_TYPE_SERIES")
  expect_error(cifti.structures(cii, dim = 1L), "CIFTI_INDEX_TYPE_PARCELS")
})


test_that("the volume fixture has voxel brain models and a transformation matrix", {
  cii <- read.cifti.header(cifti.fixture("tiny_volume.dscalar.nii"))
  map <- cii$matrix$indices_maps[[2]]
  expect_equal(map$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_equal(length(map$volumes), 1L)
  expect_equal(map$volumes[[1]]$dimensions, c(4L, 4L, 4L))
  expect_equal(map$volumes[[1]]$meter_exponent, -3L)
  expect_equal(dim(map$volumes[[1]]$transformation_matrix), c(4L, 4L))
  expect_equal(map$volumes[[1]]$transformation_matrix[1L, 1L], 2)
  expect_equal(map$volumes[[1]]$transformation_matrix[2L, 2L], 2)
  expect_equal(map$volumes[[1]]$transformation_matrix[3L, 3L], 2)
  expect_equal(map$volumes[[1]]$transformation_matrix[4L, 4L], 1)
  # The volume has no translation in this fixture.
  expect_true(all(map$volumes[[1]]$transformation_matrix[1:3, 4L] == 0))

  structures <- cifti.structures(cii, dim = 1L)
  expect_equal(nrow(structures), 3L)
  expect_equal(structures$model_type, rep("VOXELS", 3L))
  expect_equal(structures$index_count, c(32L, 1L, 30L))
  expect_equal(structures$index_offset, c(0L, 32L, 33L))
  expect_true(all(is.na(structures$surface_number_of_vertices)))

  # Voxel indices are returned as an n x 3 matrix of 0-based IJK indices.
  voxel_indices <- map$brain_models[[1]]$voxel_indices_ijk
  expect_equal(dim(voxel_indices), c(32L, 3L))
  expect_true(all(voxel_indices >= 0L))
  expect_true(all(voxel_indices <= 3L))
})


test_that("the mixed fixture contains a structure twice (surface and volume)", {
  cii <- read.cifti.header(cifti.fixture("tiny_mixed.dscalar.nii"))
  structures <- cifti.structures(cii, dim = 1L)
  expect_equal(nrow(structures), 5L)
  expect_equal(structures$structure_short, c("CORTEX_LEFT", "CORTEX_RIGHT", "CORTEX_LEFT", "CORTEX_RIGHT", "CEREBELLUM"))
  expect_equal(structures$model_type, c("SURFACE", "SURFACE", "VOXELS", "VOXELS", "VOXELS"))
  expect_equal(structures$index_offset, c(0L, 5L, 17L, 49L, 50L))
  expect_equal(structures$index_count, c(5L, 12L, 32L, 1L, 30L))
  expect_equal(sum(structures$index_count), cii$matrix$dim_sizes[2L])
  # The mixed file has 4 scalar maps, so the volume has one map per scalar.
  expect_equal(cii$matrix$dim_sizes, c(4L, 80L))
})


test_that("the dpconn and pdconn fixtures have the expected dimension order", {
  # The file type names list the dimensions in reverse order, so the dimension
  # order must be derived from the XML and never from the file name.
  dpconn <- read.cifti.header(cifti.fixture("tiny.dpconn.nii"))
  expect_equal(dpconn$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_equal(dpconn$matrix$indices_maps[[2]]$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_equal(dpconn$matrix$dim_sizes, c(22L, 3L))

  pdconn <- read.cifti.header(cifti.fixture("tiny.pdconn.nii"))
  expect_equal(pdconn$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_equal(pdconn$matrix$indices_maps[[2]]$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_equal(pdconn$matrix$dim_sizes, c(3L, 22L))
})


test_that("the parcellated scalar fixture has scalars and parcels", {
  cii <- read.cifti.header(cifti.fixture("tiny.pscalar.nii"))
  expect_equal(cii$matrix$indices_maps[[1]]$type, "CIFTI_INDEX_TYPE_SCALARS")
  expect_equal(length(cii$matrix$indices_maps[[1]]$named_maps), 4L)
  expect_equal(cii$matrix$indices_maps[[2]]$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_null(cifti.series.info(cii, dim = 0L))
})


test_that("the official example files can be read", {
  dtseries_file <- file.path("..", "..", "extra_test_data", "cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii")
  skip_if_not(file.exists(dtseries_file), "extra_test_data not available")
  cii <- read.cifti.header(dtseries_file)
  expect_equal(cii$matrix$dim_sizes, c(2L, 60951L))
  series <- cifti.series.info(cii, dim = 0L)
  expect_equal(series$number_of_series_points, 2L)
  structures <- cifti.structures(cii, dim = 1L)
  expect_equal(structures$index_count, c(30424L, 30527L))
  # The grayordinates file drops the medial wall: fewer indices than surface vertices.
  expect_equal(structures$surface_number_of_vertices, c(32492L, 32492L))
  expect_equal(sum(structures$index_count), 60951L)
  # Metadata of the official file (converted from CIFTI-1 by Workbench 0.84).
  expect_equal(names(cii$matrix$metadata)[1L], "ParentProvenance")

  ptseries_file <- file.path("..", "..", "extra_test_data", "cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii")
  cii_p <- read.cifti.header(ptseries_file)
  parcels <- cifti.parcels(cii_p, dim = 1L)
  expect_equal(nrow(parcels), 54L)
  expect_equal(parcels$name[1L], "MEDIAL.WALL")
  expect_equal(parcels$index, 0:53)

  dlabel_file <- file.path("..", "..", "extra_test_data", "cifti", "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii")
  cii_l <- read.cifti.header(dlabel_file)
  expect_equal(cii_l$matrix$dim_sizes, c(3L, 64984L))
  expect_equal(length(cii_l$matrix$indices_maps[[1]]$named_maps), 3L)
  label_table <- cifti.label.table(cii_l, dim = 0L, map = 2L)
  expect_equal(nrow(label_table), 96L)
  expect_equal(label_table$label[2L], "MEDIAL.WALL")
  expect_equal(label_table$key[1L], 0L)
})


# --- parameter validation and error messages ---------------------------------

test_that("the reader validates its arguments", {
  expect_error(read.cifti.header(1L), "must be a character string")
  expect_error(read.cifti.header(c("a", "b")), "must be a character string")
  expect_error(read.cifti.header(NA_character_), "must be a character string")
  expect_error(read.cifti.header(tempfile()), "does not exist")
  expect_error(read.cifti.header("some_file.nii"), "does not exist")
  gz_filepath <- tempfile(fileext = ".nii.gz")
  file.copy(cifti.fixture("tiny.dscalar.nii"), gz_filepath)
  expect_error(read.cifti.header(gz_filepath), "must not be compressed")
})

test_that("accessors validate their arguments", {
  cii <- read.cifti.header(cifti.fixture("tiny.dscalar.nii"))
  expect_error(cifti.structures(list()), "must be an fs.cifti instance")
  expect_error(cifti.structures(cii, dim = "1"), "single non-negative integer")
  expect_error(cifti.structures(cii, dim = -1L), "single non-negative integer")
  expect_error(cifti.structures(cii, dim = 2L), "must be in range 0 to 1")
  expect_error(cifti.parcels(cii, dim = 5L), "must be in range 0 to 1")
})

test_that("a NIFTI file that is not CIFTI is rejected with a clear error", {
  expect_error(read.cifti.header(system.file("extdata", "tiny.nii", package = "freesurferformats")), "cannot be a CIFTI-2 file")
})

test_that("a NIFTI-2 file without the CIFTI extension is rejected", {
  filepath <- tempfile(fileext = ".nii")
  niidata <- as.double(1:4)
  niiheader <- ni2header.for.data(niidata)
  niiheader$dim <- c(6L, 1L, 1L, 1L, 1L, 2L, 2L, 1L)
  write.nifti2(filepath, niidata, niiheader)
  expect_error(read.cifti.header(filepath), "no NIFTI v2 header extension with code 32")
})

test_that("a truncated CIFTI-2 file is rejected", {
  filepath <- tempfile(fileext = ".nii")
  fixture_bytes <- readBin(cifti.fixture("tiny.dscalar.nii"), raw(), n = 4000L)
  writeBin(fixture_bytes, filepath)
  expect_error(suppressWarnings(read.cifti.header(filepath)), "no NIFTI v2 header extension with code 32")
})

test_that("a CIFTI-1 file is rejected with a hint", {
  # A CIFTI-1 file is a NIFTI-1 file that contains CIFTI XML, so a file with a
  # NIFTI-1 header that contains the string 'CIFTI' is a good stand-in for one.
  filepath <- tempfile(fileext = ".dtseries.nii")
  file.copy(system.file("extdata", "tiny.nii", package = "freesurferformats"), filepath)
  cat('<CIFTI Version="1.0">', file = filepath, append = TRUE)
  expect_error(read.cifti.header(filepath), "looks like a CIFTI-1 file")
})

test_that("gzipped CIFTI files are rejected", {
  # The file content does not matter, the check is based on the file name.
  filepath <- tempfile(fileext = ".gz")
  file.copy(cifti.fixture("tiny.dscalar.nii"), filepath)
  expect_error(read.cifti.header(filepath), "must not be compressed")
})

test_that("inconsistent XML is rejected: CIFTI version, root element and malformed XML", {
  xml_v1 <- cifti.test.xml(cifti.test.map.scalars(), version = "1")
  expect_error(read.cifti.header(cifti.test.file(xml_v1, dim_sizes = c(1L, 1L))), "only CIFTI-2 is supported")

  xml_no_version <- paste0('<?xml version="1.0"?><CIFTI><Matrix>', cifti.test.map.scalars(), cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L)), "</Matrix></CIFTI>")
  expect_error(read.cifti.header(cifti.test.file(xml_no_version, dim_sizes = c(1L, 1L))), "no 'Version' attribute")

  xml_wrong_root <- cifti.test.xml(cifti.test.map.scalars(), root = "CIFTIX")
  expect_error(read.cifti.header(cifti.test.file(xml_wrong_root, dim_sizes = c(1L, 1L))), "expected 'CIFTI'")

  xml_malformed <- paste0('<?xml version="1.0"?><CIFTI Version="2"><Matrix><MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS">')
  expect_error(read.cifti.header(cifti.test.file(xml_malformed, dim_sizes = c(1L, 1L))), "Failed to parse the XML metadata")

  xml_no_matrix <- '<?xml version="1.0"?><CIFTI Version="2"></CIFTI>'
  expect_error(read.cifti.header(cifti.test.file(xml_no_matrix, dim_sizes = c(1L, 1L))), "contains no 'Matrix' element")
})

test_that("inconsistent XML is rejected: unknown types", {
  xml_unknown_type <- cifti.test.xml('<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_MAGIC"/>')
  expect_error(read.cifti.header(cifti.test.file(xml_unknown_type, dim_sizes = c(1L, 1L))), "invalid or unsupported 'IndicesMapToDataType'")

  xml_model_required <- cifti.test.xml('<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"><BrainModel IndexOffset="0" IndexCount="1" BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ModelType="CIFTI_MODEL_TYPE_INVALID"><VertexIndices>0</VertexIndices></BrainModel></MatrixIndicesMap>')
  expect_error(read.cifti.header(cifti.test.file(xml_model_required, dim_sizes = c(1L, 1L))), "invalid or unsupported 'ModelType'")

  # A missing required attribute is reported as well.
  xml_no_offset <- cifti.test.xml('<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"><BrainModel IndexCount="1" BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ModelType="CIFTI_MODEL_TYPE_SURFACE" SurfaceNumberOfVertices="1"><VertexIndices>0</VertexIndices></BrainModel></MatrixIndicesMap>')
  expect_error(read.cifti.header(cifti.test.file(xml_no_offset, dim_sizes = c(1L, 1L))), "required attribute 'IndexOffset' is missing")
})

test_that("inconsistent XML is rejected: dimension coverage", {
  # Two mappings for the same dimension.
  xml_dup <- cifti.test.xml(paste0(cifti.test.map.scalars(), cifti.test.map.scalars()))
  expect_error(read.cifti.header(cifti.test.file(xml_dup, dim_sizes = c(1L, 1L))), "more than one mapping")

  # No mapping for dimension 1.
  xml_missing_dim <- cifti.test.xml(cifti.test.map.scalars())
  expect_error(read.cifti.header(cifti.test.file(xml_missing_dim, dim_sizes = c(1L, 1L))), "no mapping for matrix dimension 1")

  # A mapping for a dimension that does not exist.
  xml_bad_dim <- cifti.test.xml(paste0(cifti.test.map.scalars(), '<MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"/>', '<MatrixIndicesMap AppliesToMatrixDimension="2" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"/>'))
  expect_error(read.cifti.header(cifti.test.file(xml_bad_dim, dim_sizes = c(1L, 1L))), "matrix dimension 2")
})

test_that("inconsistent XML is rejected: brain model index ranges", {
  xml_gap <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(paste0(
      cifti.test.bm.surface(0L, 2L, vertices = c(0L, 1L)),
      cifti.test.bm.surface(3L, 2L, structure = "CIFTI_STRUCTURE_CORTEX_RIGHT", vertices = c(2L, 3L))
    ))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_gap, dim_sizes = c(1L, 4L))), "without gaps or overlaps")

  xml_too_short <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 3L, vertices = c(0L, 1L, 2L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_too_short, dim_sizes = c(1L, 4L))), "cover 3 matrix entries")

  xml_count_mismatch <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 3L, num_verts = 4L, vertices = c(0L, 1L, 2L, 3L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_count_mismatch, dim_sizes = c(1L, 3L))), "but the model declares 3 indices")

  xml_out_of_range <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 3L, num_verts = 3L, vertices = c(0L, 1L, 3L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_out_of_range, dim_sizes = c(1L, 3L))), "out of range")

  # No VertexIndices element means 'all vertices', so IndexCount must equal SurfaceNumberOfVertices.
  xml_no_vertices <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 3L, num_verts = 4L))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_no_vertices, dim_sizes = c(1L, 3L))), "must cover all 4 vertices")

  xml_no_num_verts <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models('<BrainModel IndexOffset="0" IndexCount="3" BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ModelType="CIFTI_MODEL_TYPE_SURFACE"><VertexIndices>0 1 2</VertexIndices></BrainModel>')
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_no_num_verts, dim_sizes = c(1L, 3L))), "without the required 'SurfaceNumberOfVertices' attribute")

  # A brain models mapping without any brain model entry.
  xml_no_models <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    '<MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"/>'
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_no_models, dim_sizes = c(1L, 3L))), "without any 'BrainModel' entries")
})

test_that("inconsistent XML is rejected: volume models", {
  # A volume brain model without a Volume element cannot be interpreted.
  xml_no_volume <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 1L, voxels = c(0L, 0L, 0L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_no_volume, dim_sizes = c(1L, 1L))), "no 'Volume' element")

  # The number of voxel indices must match the declared index count.
  xml_voxel_count <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 3L, voxels = c(0L, 0L, 0L)), cifti.test.volume(c(2L, 2L, 2L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_voxel_count, dim_sizes = c(1L, 3L))), "but the model declares 3 indices")

  # Voxel indices outside of the volume.
  xml_voxel_range <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 2L, voxels = c(0L, 0L, 0L, 0L, 0L, 5L)), cifti.test.volume(c(2L, 2L, 2L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_voxel_range, dim_sizes = c(1L, 2L))), "out of range")

  # A VoxelIndicesIJK element with a number of values that is not a multiple of 3.
  xml_voxel_length <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 1L, voxels = c(0L, 0L)), cifti.test.volume(c(2L, 2L, 2L)))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_voxel_length, dim_sizes = c(1L, 1L))), "not a multiple of 3")

  # A Volume element without a transformation matrix.
  xml_volume_no_matrix <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 1L, voxels = c(0L, 0L, 0L)), '<Volume VolumeDimensions="2,2,2"/>')
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_volume_no_matrix, dim_sizes = c(1L, 1L))), "no 'TransformationMatrixVoxelIndicesIJKtoXYZ'")

  # A transformation matrix with the wrong number of values.
  xml_volume_matrix_short <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 1L, voxels = c(0L, 0L, 0L)), '<Volume VolumeDimensions="2,2,2"><TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">1 0 0 0</TransformationMatrixVoxelIndicesIJKtoXYZ></Volume>')
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_volume_matrix_short, dim_sizes = c(1L, 1L))), "4 values instead of 16")
})

test_that("inconsistent XML is rejected: series, parcels and labels", {
  # A series mapping without series attributes.
  xml_series_no_attrs <- cifti.test.xml(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES"/>',
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_series_no_attrs, dim_sizes = c(1L, 1L))), "without series attributes")

  # A series length that does not match the matrix dimension.
  xml_series_count <- cifti.test.xml(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES" NumberOfSeriesPoints="3" SeriesStart="0" SeriesStep="1" SeriesUnit="SECOND"/>',
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_series_count, dim_sizes = c(1L, 1L))), "declares 3 series points")

  # Incomplete series attributes.
  xml_series_incomplete <- cifti.test.xml(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES" NumberOfSeriesPoints="1" SeriesStart="0"/>',
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_series_incomplete, dim_sizes = c(1L, 1L))), "missing one of the required attributes")

  # A parcels mapping with the wrong number of parcels.
  xml_parcel_count <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    '<MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_PARCELS"><Parcel Name="A"><Vertices BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT">0</Vertices></Parcel></MatrixIndicesMap>'
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_parcel_count, dim_sizes = c(1L, 2L))), "declares 1 parcels")

  # A parcel with vertices outside of the declared surface.
  xml_parcel_range <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    '<MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_PARCELS"><Surface BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" SurfaceNumberOfVertices="3"/><Parcel Name="A"><Vertices BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT">0 5</Vertices></Parcel></MatrixIndicesMap>'
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_parcel_range, dim_sizes = c(1L, 1L))), "out of range")

  # A label mapping with a number of named maps that does not match the dimension.
  xml_label_count <- cifti.test.xml(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_LABELS"><NamedMap><MapName>a</MapName></NamedMap><NamedMap><MapName>b</MapName></NamedMap></MatrixIndicesMap>',
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))
  ))
  expect_error(read.cifti.header(cifti.test.file(xml_label_count, dim_sizes = c(1L, 1L))), "declares 2 label maps")

  # A label mapping without any named map.
  xml_label_none <- cifti.test.xml('<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_LABELS"/><MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"><BrainModel IndexOffset="0" IndexCount="1" BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ModelType="CIFTI_MODEL_TYPE_SURFACE" SurfaceNumberOfVertices="1"><VertexIndices>0</VertexIndices></BrainModel></MatrixIndicesMap>')
  expect_error(read.cifti.header(cifti.test.file(xml_label_none, dim_sizes = c(1L, 1L))), "without any 'NamedMap' entries")
})

test_that("a parcellation may not refer to an undeclared surface, but only warns", {
  xml_parcel_undeclared <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    '<MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_PARCELS"><Parcel Name="A"><Vertices BrainStructure="CIFTI_STRUCTURE_CORTEX_RIGHT">0</Vertices></Parcel></MatrixIndicesMap>'
  ))
  expect_warning(read.cifti.header(cifti.test.file(xml_parcel_undeclared, dim_sizes = c(1L, 1L))), "declares no surface")
})

test_that("a scalars mapping with a mismatching number of named maps warns", {
  xml_scalar_names <- cifti.test.xml(paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"><NamedMap><MapName>only_one</MapName></NamedMap></MatrixIndicesMap>',
    cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))
  ))
  expect_warning(read.cifti.header(cifti.test.file(xml_scalar_names, dim_sizes = c(2L, 1L))), "some maps are unnamed")
})

test_that("non-empty dim[1] to dim[4] fields cause a warning", {
  filepath <- tempfile(fileext = ".nii")
  niidata <- as.double(1L)
  niiheader <- ni2header.for.data(niidata)
  niiheader$dim <- c(6L, 1L, 1L, 1L, 2L, 1L, 1L, 1L)
  xml <- cifti.test.xml(paste0(cifti.test.map.scalars(), cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))))
  write.nifti2(filepath, niidata, niiheader, extensions = list(nifti2.extension(32L, xml)))
  expect_warning(read.cifti.header(filepath), "non-empty dim")
})


# --- value parsing details ---------------------------------------------------

test_that("all voxel indices on a single line are supported (nibabel layout)", {
  # Connectome Workbench writes one voxel per line, nibabel writes all indices of
  # an element on one line. Both must be accepted: the 'cifti' package only
  # supports the Workbench layout and therefore fails on every voxel model file
  # written by nibabel.
  voxels <- c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L)
  xml_one_line <- cifti.test.xml(paste0(
    cifti.test.map.scalars(),
    cifti.test.map.brain.models(cifti.test.bm.volume(0L, 3L, voxels = voxels), cifti.test.volume(c(2L, 2L, 2L)))
  ))
  cii <- read.cifti.header(cifti.test.file(xml_one_line, dim_sizes = c(1L, 3L)))
  expect_equal(cii$matrix$indices_maps[[2]]$brain_models[[1]]$voxel_indices_ijk,
               matrix(voxels, ncol = 3L, byrow = TRUE))

  xml_multi_line <- sub(paste(voxels, collapse = " "), paste(voxels, collapse = "\n"), xml_one_line, fixed = TRUE)
  cii2 <- read.cifti.header(cifti.test.file(xml_multi_line, dim_sizes = c(1L, 3L)))
  expect_equal(cii$matrix$indices_maps[[2]]$brain_models[[1]]$voxel_indices_ijk,
               cii2$matrix$indices_maps[[2]]$brain_models[[1]]$voxel_indices_ijk)
})

test_that("metadata entries are returned in file order and may contain XML", {
  metadata <- paste0(
    "<MD><Name>First</Name><Value>one</Value></MD>",
    "<MD><Name>Second</Name><Value>two</Value></MD>",
    "<MD><Name>PaletteColorMapping</Name><Value>&lt;PaletteColorMapping Version=\"1\"/&gt;</Value></MD>"
  )
  xml <- cifti.test.xml(paste0(cifti.test.map.scalars(), cifti.test.map.brain.models(cifti.test.bm.surface(0L, 1L, num_verts = 1L))), metadata = metadata)
  cii <- read.cifti.header(cifti.test.file(xml, dim_sizes = c(1L, 1L)))
  expect_equal(names(cii$matrix$metadata), c("First", "Second", "PaletteColorMapping"))
  expect_equal(cii$matrix$metadata$First, "one")
  # The escaped XML in the metadata value is unescaped when reading.
  expect_equal(cii$matrix$metadata$PaletteColorMapping, '<PaletteColorMapping Version="1"/>')
})

test_that("a file with three matrix dimensions can be read", {
  xml <- '<?xml version="1.0"?><CIFTI Version="2"><Matrix><MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"/><MatrixIndicesMap AppliesToMatrixDimension="1,2" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS"/></Matrix></CIFTI>'
  filepath <- tempfile(fileext = ".nii")
  niidata <- as.double(1:4)
  niiheader <- ni2header.for.data(niidata)
  niiheader$dim <- c(7L, 1L, 1L, 1L, 1L, 1L, 2L, 2L)
  write.nifti2(filepath, niidata, niiheader, extensions = list(nifti2.extension(32L, xml)))
  cii <- read.cifti.header(filepath)
  expect_equal(cii$matrix$dim_sizes, c(1L, 2L, 2L))
  expect_equal(cii$matrix$indices_maps[[2]]$dims, c(1L, 2L))
  expect_equal(cii$matrix$indices_maps[[2]]$size, c(2L, 2L))
  expect_error(cifti.structures(cii, dim = 2L), "not 'CIFTI_INDEX_TYPE_BRAIN_MODELS'")
})

test_that("the brain structure name normalization handles all spellings", {
  expect_equal(cifti.structure.canonical("CIFTI_STRUCTURE_CORTEX_LEFT"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(cifti.structure.canonical("CortexLeft"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(cifti.structure.canonical("lh"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(cifti.structure.canonical("left"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(cifti.structure.canonical("rh"), "CIFTI_STRUCTURE_CORTEX_RIGHT")
  expect_equal(cifti.structure.canonical("CortexRight"), "CIFTI_STRUCTURE_CORTEX_RIGHT")
  expect_equal(cifti.structure.canonical("BrainStem"), "CIFTI_STRUCTURE_BRAIN_STEM")
  expect_equal(cifti.structure.canonical("CIFTI_STRUCTURE_CEREBELLUM"), "CIFTI_STRUCTURE_CEREBELLUM")
  expect_true(is.na(cifti.structure.canonical(NA_character_)))
  expect_equal(cifti.structure.short("CIFTI_STRUCTURE_ACCUMBENS_LEFT"), "ACCUMBENS_LEFT")
  expect_error(cifti.structure.canonical(c("lh", "rh")), "single character string")
})

test_that("the print method works", {
  cii <- read.cifti.header(cifti.fixture("tiny.dtseries.nii"))
  output <- capture.output(print(cii))
  expect_true(any(grepl("CIFTI-2 file", output)))
  expect_true(any(grepl("series", output)))
  expect_true(any(grepl("brain model entries", output)))
  # The print method returns its argument invisibly.
  invisible_output <- capture.output(returned <- print(cii))
  expect_true(inherits(returned, "fs.cifti"))

  output_parcels <- capture.output(print(read.cifti.header(cifti.fixture("tiny.pconn.nii"))))
  expect_true(any(grepl("parcels", output_parcels)))
})
