# Tests for the CIFTI-2 writer (R/write_cifti*.R).
#
# The fixtures in inst/extdata/cifti are used both as input (their axes are read with
# cifti.axis.from.template()) and as the reference for a round trip: a file written from
# the axes and data of a fixture has to read back as the same file (same data, same axes,
# same intent code). The files this package writes are additionally verified against
# nibabel and Connectome Workbench by dev_tools/check_cifti_conversion.R, which is not part
# of the test suite because it needs those tools installed.

cifti.test.write.file <- function(filename) {
  return(file.path(tempdir(), filename))
}

cifti.test.surface.axis <- function() {
  return(cifti.axis.brain.models(list(
    cifti.brain.model.surface("lh", 10L),
    cifti.brain.model.surface("rh", 12L)
  )))
}

test_that("The brain model axis builder computes the index ranges", {
  axis <- cifti.test.surface.axis()
  expect_equal(axis$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_length(axis$brain_models, 2L)
  expect_equal(vapply(axis$brain_models, function(model) model$index_offset, integer(1L)), c(0L, 10L))
  expect_equal(vapply(axis$brain_models, function(model) model$index_count, integer(1L)), c(10L, 12L))
  expect_equal(cifti.axis.size(axis), 22L)
  expect_equal(axis$brain_models[[1L]]$brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")

  # A model with an explicit index list covers as many entries as the list is long.
  roi_axis <- cifti.axis.brain.models(list(
    cifti.brain.model.surface("lh", 10L, vertices = c(0L, 2L, 4L)),
    cifti.brain.model.surface("rh", 12L, vertices = 0:5)
  ))
  expect_equal(cifti.axis.size(roi_axis), 9L)
  expect_equal(vapply(roi_axis$brain_models, function(model) model$index_offset, integer(1L)), c(0L, 3L))

  # A single model entry can be passed directly.
  single <- cifti.axis.brain.models(cifti.brain.model.surface("lh", 10L))
  expect_length(single$brain_models, 1L)
  expect_equal(cifti.axis.size(single), 10L)

  # The index lists are validated.
  expect_error(cifti.brain.model.surface("lh", 10L, vertices = c(0L, 10L)), "indices above 9")
  expect_error(cifti.brain.model.surface("lh", 10L, vertices = c(0L, -1L)), "non-negative")
  expect_error(cifti.brain.model.surface("lh", 10L, vertices = c(1L, 1L)), "duplicate indices")
  expect_error(cifti.axis.brain.models(list()), "must be a list of brain model entries")
  expect_error(cifti.brain.model.surface("lh", NA), "positive integer")
})

test_that("The volume axis builder checks the transformation matrix", {
  volume <- cifti.volume(c(4L, 4L, 4L), diag(c(2, 2, 2, 1)))
  expect_equal(volume$dimensions, c(4L, 4L, 4L))
  expect_equal(volume$meter_exponent, -3L)
  expect_equal(dim(volume$transformation_matrix), c(4L, 4L))

  expect_error(cifti.volume(c(4L, 4L), diag(4)), "length 3")
  expect_error(cifti.volume(c(4L, 4L, 4L), diag(3)), "4x4 matrix")

  voxels <- matrix(c(0L, 0L, 2L, 1L, 0L, 2L), ncol = 3L, byrow = TRUE)
  model <- cifti.brain.model.volume("CEREBELLUM", voxels)
  expect_equal(nrow(model$voxel_indices_ijk), 2L)
  expect_equal(model$model_type, "CIFTI_MODEL_TYPE_VOXELS")
  expect_true(is.na(model$surface_number_of_vertices))

  # A volume model needs the volume: its voxel indices cannot be interpreted without it.
  expect_error(cifti.axis.brain.models(list(model)), "needs the volume they refer to")
  axis <- cifti.axis.brain.models(list(model), volume = volume)
  expect_equal(length(axis$volumes), 1L)
  expect_error(cifti.brain.model.volume("X", c(1L, 2L)), "3 values")

  # A model without voxel indices covers the whole volume, which needs the volume size.
  all_voxels <- cifti.axis.brain.models(list(cifti.brain.model.volume("CEREBELLUM")), volume = volume)
  expect_equal(cifti.axis.size(all_voxels), 64L)
})

test_that("The parcel and named map axis builders work", {
  parcels <- cifti.axis.parcels(list(
    cifti.parcel("PARCEL_A", list(CORTEX_LEFT = 0:2, CORTEX_RIGHT = 0:1)),
    cifti.parcel("PARCEL_B", list(CORTEX_LEFT = 3:5))
  ))
  expect_equal(parcels$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_equal(cifti.axis.size(parcels), 2L)
  expect_equal(parcels$parcels[[1L]]$index, 0L)
  expect_equal(names(parcels$parcels[[1L]]$vertices), c("CORTEX_LEFT", "CORTEX_RIGHT"))

  # A single parcel can be passed directly.
  expect_equal(cifti.axis.size(cifti.axis.parcels(cifti.parcel("A", list(CORTEX_LEFT = 1L)))), 1L)
  expect_error(cifti.parcel(""), "non-empty character string")
  expect_error(cifti.parcel("A", 1:3), "named list")
  expect_error(cifti.parcel("A", list(1:3)), "named list")

  scalars <- cifti.axis.scalars(c("thickness", "area"))
  expect_equal(scalars$type, "CIFTI_INDEX_TYPE_SCALARS")
  expect_equal(cifti.axis.size(scalars), 2L)
  expect_equal(scalars$named_maps[[2L]]$name, "area")

  label_table <- data.frame(key = 0:1, red = c(1, 0), green = c(1, 0.5), blue = c(1, 1),
                            alpha = c(0, 1), label = c("???", "PARCEL_A"), stringsAsFactors = FALSE)
  labels <- cifti.axis.labels("parcellation", label_tables = list(label_table))
  expect_equal(labels$type, "CIFTI_INDEX_TYPE_LABELS")
  expect_equal(labels$named_maps[[1L]]$labels$label, c("???", "PARCEL_A"))

  expect_error(cifti.axis.scalars(), "number of maps in the dimension is the number of names")
  expect_error(cifti.axis.labels(c("a", "b"), label_tables = list(label_table)), "one entry per map")
  expect_error(cifti.axis.labels("a", label_tables = list(data.frame(key = 1L))), "missing the columns")
  expect_error(cifti.axis.labels("a", label_tables = list(label_table[1:2, ])), NA)
  bad_table <- label_table
  bad_table$red <- c(2, 0)
  expect_error(cifti.axis.labels("a", label_tables = list(bad_table)), "range 0 to 1")
  bad_keys <- label_table
  bad_keys$key <- c(1L, 1L)
  expect_error(cifti.axis.labels("a", label_tables = list(bad_keys)), "duplicate label keys")
})

test_that("The series axis builder validates its input", {
  axis <- cifti.axis.series(4L, start = 0, step = 2.5)
  expect_equal(axis$type, "CIFTI_INDEX_TYPE_SERIES")
  expect_equal(axis$series$number_of_series_points, 4L)
  expect_equal(axis$series$step, 2.5)
  expect_equal(axis$series$unit, "SECOND")
  expect_equal(cifti.axis.size(axis), 4L)

  expect_error(cifti.axis.series(0L), "positive integer")
  expect_error(cifti.axis.series(4L, unit = "PARSEC"), "unit")
  expect_error(cifti.axis.series(4L, step = NA), "single numbers")
})

test_that("Axes can be read from a template file and written back", {
  for (filename in c("tiny.dscalar.nii", "tiny.dtseries.nii", "tiny.dlabel.nii", "tiny.dconn.nii",
                     "tiny.pconn.nii", "tiny.pdconn.nii", "tiny.dpconn.nii", "tiny.pscalar.nii",
                     "tiny.ptseries.nii", "tiny_roi.dscalar.nii", "tiny_volume.dscalar.nii",
                     "tiny_volume.dtseries.nii", "tiny_mixed.dscalar.nii")) {
    cifti_file <- system.file("extdata", "cifti", filename, package = "freesurferformats")
    cii <- read.cifti.header(cifti_file)

    axes <- cifti.axis.from.template(cifti_file)
    expect_length(axes, 2L)
    expect_equal(cifti.axis.size(axes[[1L]]), cii$matrix$dim_sizes[1L])
    expect_equal(cifti.axis.size(axes[[2L]]), cii$matrix$dim_sizes[2L])

    # The XML written from the axes parses back into the same matrix description.
    xml <- cifti.header.from.axes(axes, metadata = cii$matrix$metadata)
    reparsed <- cifti.parse.xml(xml, niiheader = cii$niiheader)
    expect_equal(reparsed$matrix, cii$matrix)

    # A file written from the axes and the data of the fixture is the same file.
    out_file <- cifti.test.write.file(paste0("rewritten_", filename))
    write.cifti(out_file, read.cifti(cifti_file)$data, template = cifti_file)
    rewritten <- read.cifti(out_file)
    expect_equal(unname(rewritten$data), unname(read.cifti(cifti_file)$data))
    expect_equal(rewritten$header$matrix$indices_maps, cii$matrix$indices_maps)
    expect_equal(rewritten$header$niiheader$intent_code, cii$niiheader$intent_code)
    expect_equal(rewritten$header$matrix$metadata, cii$matrix$metadata)
    unlink(out_file)
  }
})

test_that("We can write the standard file types from scratch", {
  # dscalar: one map, brain models in the second dimension.
  dscalar_file <- cifti.test.write.file("scratch.dscalar.nii")
  write.cifti(dscalar_file, matrix(1:22, nrow = 1L),
              axes = list(cifti.axis.scalars("mymap"), cifti.test.surface.axis()),
              metadata = c("Provenance" = "unit test"))
  cii <- read.cifti(dscalar_file)
  expect_equal(dim(cii$data), c(1L, 22L))
  expect_equal(unname(cii$data[1L, 1:5]), c(1, 2, 3, 4, 5))
  expect_equal(cii$header$niiheader$intent_code, 3006L)
  expect_equal(trimws(cii$header$niiheader$intent_name), "ConnDenseScalar")
  expect_equal(cii$header$matrix$metadata$Provenance, "unit test")
  expect_equal(cifti.dim.labels(cii$header, 0L), "mymap")
  expect_equal(cifti.structures(cii$header, 1L)$index_count, c(10L, 12L))

  # dtseries: a series in the first dimension.
  dtseries_file <- cifti.test.write.file("scratch.dtseries.nii")
  series_data <- matrix(seq_len(3L * 22L), nrow = 3L)
  write.cifti(dtseries_file, series_data,
              axes = list(cifti.axis.series(3L, start = 0, step = 0.72), cifti.test.surface.axis()))
  cii <- read.cifti(dtseries_file)
  expect_equal(unname(cii$data), series_data)
  expect_equal(cii$header$niiheader$intent_code, 3002L)
  expect_equal(cifti.series.info(cii$header, 0L)$step, 0.72)

  # dlabel: a label map with a label table.
  dlabel_file <- cifti.test.write.file("scratch.dlabel.nii")
  label_table <- data.frame(key = 0:2, red = c(1, 0, 0), green = c(1, 0.5, 0.5), blue = c(1, 1, 0),
                            alpha = c(0, 1, 1), label = c("???", "A", "B"), stringsAsFactors = FALSE)
  label_keys <- matrix(c(rep(1L, 10L), rep(2L, 12L)), nrow = 1L)
  write.cifti(dlabel_file, label_keys,
              axes = list(cifti.axis.labels("parc", label_tables = list(label_table)), cifti.test.surface.axis()))
  cii <- read.cifti(dlabel_file)
  expect_equal(cii$header$niiheader$intent_code, 3007L)
  expect_equal(cifti.label.table(cii$header, 0L, 1L)$label, c("???", "A", "B"))
  expect_equal(as.integer(read.fs.parcellation.cifti(dlabel_file, "lh")), rep(1L, 10L))

  # dconn: identical axes in both dimensions are written as a single mapping, like
  # Connectome Workbench does it.
  dconn_file <- cifti.test.write.file("scratch.dconn.nii")
  write.cifti(dconn_file, diag(22L), axes = list(cifti.test.surface.axis(), cifti.test.surface.axis()))
  cii <- read.cifti(dconn_file)
  expect_equal(dim(cii$data), c(22L, 22L))
  expect_length(cii$header$matrix$indices_maps, 1L)
  expect_equal(cii$header$matrix$indices_maps[[1L]]$dims, c(0L, 1L))
  expect_equal(cii$header$niiheader$intent_code, 3001L)
  expect_equal(unname(diag(read.cifti(dconn_file)$data)), rep(1, 22L))

  # pconn: parcels in both dimensions. The surfaces are declared here as well, which
  # makes the vertex indices of the parcels validatable (and is what the files that
  # Connectome Workbench writes contain).
  pconn_file <- cifti.test.write.file("scratch.pconn.nii")
  parcel_surfaces <- c(CORTEX_LEFT = 10L, CORTEX_RIGHT = 12L)
  parcel_axis <- cifti.axis.parcels(list(
    cifti.parcel("PARCEL_A", list(CORTEX_LEFT = 0:4)),
    cifti.parcel("PARCEL_B", list(CORTEX_LEFT = 5:9))
  ), surfaces = parcel_surfaces)
  expect_silent(write.cifti(pconn_file, matrix(1:4, nrow = 2L), axes = list(parcel_axis, parcel_axis)))
  cii <- read.cifti(pconn_file)
  expect_equal(cii$header$niiheader$intent_code, 3003L)
  expect_equal(cifti.parcels(cii$header, 1L)$name, c("PARCEL_A", "PARCEL_B"))
  expect_length(cii$header$matrix$indices_maps, 1L)
  expect_equal(cifti.axis.surface.sizes(cii$header$matrix$indices_maps[[1L]]), parcel_surfaces)

  # A volume brain model with the volume it refers to.
  volume_file <- cifti.test.write.file("scratch_volume.dscalar.nii")
  volume <- cifti.volume(c(4L, 4L, 4L), diag(c(2, 2, 2, 1)), meter_exponent = -3L)
  voxels <- matrix(c(0L, 0L, 2L, 1L, 0L, 2L, 0L, 1L, 2L), ncol = 3L, byrow = TRUE)
  write.cifti(volume_file, matrix(c(1, 2, 3), nrow = 1L),
              axes = list(cifti.axis.scalars("mymap"),
                          cifti.axis.brain.models(list(cifti.brain.model.volume("CEREBELLUM", voxels)), volume = volume)))
  cii <- read.cifti(volume_file)
  structure_data <- cifti.structure.data(cii, "CEREBELLUM")
  expect_equal(unname(structure_data$volume$voxel_indices_ijk), unname(voxels))
  expect_equal(unname(structure_data$volume$values[, 1L]), c(1, 2, 3))
  expect_equal(structure_data$volume$dimensions, c(4L, 4L, 4L))
  expect_equal(structure_data$volume$transformation_matrix, diag(c(2, 2, 2, 1)))
})

test_that("The writer validates the data, the axes and the file name", {
  surface_axis <- cifti.test.surface.axis()
  out_file <- cifti.test.write.file("validation.dscalar.nii")

  # Data of the wrong size.
  expect_error(write.cifti(out_file, matrix(1:10, nrow = 1L),
                           axes = list(cifti.axis.scalars("m"), surface_axis)),
               "axes describe a matrix of size 1 x 22")
  # A vector of the right length is accepted and interpreted as an array.
  expect_error(write.cifti(out_file, 1:22, axes = list(cifti.axis.scalars("m"), surface_axis)), NA)
  # A file name that contradicts the axes.
  expect_error(write.cifti(cifti.test.write.file("wrong.pconn.nii"), matrix(1:22, nrow = 1L),
                           axes = list(cifti.axis.scalars("m"), surface_axis)),
               "names the CIFTI-2 file type")
  # Missing axes.
  expect_error(write.cifti(out_file, matrix(1:22, nrow = 1L)), "Either 'axes' or 'template'")
  # Gzipped output.
  expect_error(write.cifti(cifti.test.write.file("zipped.dscalar.nii.gz"), matrix(1:22, nrow = 1L),
                           axes = list(cifti.axis.scalars("m"), surface_axis)),
               "must not be compressed")
  # An axis of an unknown type.
  expect_error(write.cifti(out_file, matrix(1:22, nrow = 1L),
                           axes = list(list(type = "CIFTI_INDEX_TYPE_NONSENSE"), surface_axis)),
               "unknown type")
  # Invalid file paths.
  expect_error(write.cifti(42L, matrix(1:22, nrow = 1L), axes = list(cifti.axis.scalars("m"), surface_axis)),
               "must be a character string")
  expect_error(write.cifti(c("a.nii", "b.nii"), matrix(1:22, nrow = 1L),
                           axes = list(cifti.axis.scalars("m"), surface_axis)),
               "must be a character string")

  # Axes that are not one of the nine standard file types get a warning.
  expect_warning(write.cifti(cifti.test.write.file("unusual.nii"), matrix(1:4, nrow = 2L),
                             axes = list(cifti.axis.series(2L), cifti.axis.series(2L))),
                 "not one of the nine standard CIFTI-2 file types")
})

test_that("We can write morphometry data with and without a template", {
  template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
  morph_lh <- read.fs.morph.cifti(template, "lh")
  morph_lh[2L] <- 555

  # With a template: the mapping of the file decides which vertices are written.
  out_file <- cifti.test.write.file("morph.dscalar.nii")
  write.fs.morph.cifti(out_file, morph_lh, template = template, structure = "lh")
  expect_equal(as.numeric(read.fs.morph.cifti(out_file, "lh")), morph_lh)
  expect_equal(dim(read.cifti(out_file)$data), c(1L, 10L)) # only the lh grayordinates
  unlink(out_file)

  # Both hemispheres, as a named list.
  out_file <- cifti.test.write.file("morph_both.dscalar.nii")
  morph_rh <- read.fs.morph.cifti(template, "rh")
  write.fs.morph.cifti(out_file, list(lh = morph_lh, rh = morph_rh), template = template)
  expect_equal(as.numeric(read.fs.morph.cifti(out_file, "both")), c(morph_lh, morph_rh))
  expect_equal(dim(read.cifti(out_file)$data), c(1L, 22L))
  unlink(out_file)

  # Several maps, with names.
  out_file <- cifti.test.write.file("morph_maps.dscalar.nii")
  maps <- cbind(morph_lh, morph_lh * 2)
  write.fs.morph.cifti(out_file, maps, template = template, structure = "lh", map_names = c("one", "two"))
  cii <- read.cifti(out_file)
  expect_equal(dim(cii$data), c(2L, 10L))
  expect_equal(cifti.dim.labels(cii$header, 0L), c("one", "two"))
  expect_equal(as.numeric(cii$data[2L, ]), morph_lh * 2)
  unlink(out_file)

  # Without a template the file covers all vertices of the surface.
  out_file <- cifti.test.write.file("morph_notemplate.dscalar.nii")
  write.fs.morph.cifti(out_file, 1:10, structure = "lh")
  expect_equal(dim(read.cifti(out_file)$data), c(1L, 10L))
  expect_equal(as.numeric(read.fs.morph.cifti(out_file, "lh")), as.numeric(1:10))
  unlink(out_file)

  # Errors: no structure, unknown structure, missing data for a template structure.
  expect_error(write.fs.morph.cifti(cifti.test.write.file("x.dscalar.nii"), 1:10), "the brain structure they belong to is unknown")
  expect_error(write.fs.morph.cifti(cifti.test.write.file("x.dscalar.nii"), 1:10, template = template,
                                    structure = "CEREBELLUM"), "contains no brainordinates")
  expect_error(write.fs.morph.cifti(cifti.test.write.file("x.dscalar.nii"), 1:5, template = template,
                                    structure = "lh"), "only 5 vertices")
  # A volume template cannot be used for per-vertex data.
  volume_template <- system.file("extdata", "cifti", "tiny_volume.dscalar.nii", package = "freesurferformats")
  expect_error(write.fs.morph.cifti(cifti.test.write.file("x.dscalar.nii"), 1:32, template = volume_template,
                                    structure = "lh"), "has volume voxels")
  # NA values cause a warning, since the format has no missing value.
  expect_warning(write.fs.morph.cifti(cifti.test.write.file("na.dscalar.nii"), c(1:9, NA), structure = "lh"),
                 "contain NA values")
})

test_that("We can write time series data", {
  template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
  series <- matrix(as.numeric(seq_len(10L * 3L)), nrow = 10L)

  out_file <- cifti.test.write.file("series.dtseries.nii")
  write.fs.series.cifti(out_file, series, template = template, structure = "lh", start = 0, step = 0.72)
  read_back <- read.fs.series.cifti(out_file, "lh")
  expect_equal(unname(read_back), series)
  info <- cifti.series.info(read.cifti.header(out_file), 0L)
  expect_equal(info$number_of_series_points, 3L)
  expect_equal(info$step, 0.72)
  expect_equal(info$unit, "SECOND")

  # A dtseries template (whose first dimension is a series) works as well.
  dtseries_template <- system.file("extdata", "cifti", "tiny.dtseries.nii", package = "freesurferformats")
  out_file <- cifti.test.write.file("series_from_series.dtseries.nii")
  write.fs.series.cifti(out_file, series, template = dtseries_template, structure = "lh", step = 1.5)
  expect_equal(unname(read.fs.series.cifti(out_file, "lh")), series)
  expect_equal(cifti.series.info(read.cifti.header(out_file), 0L)$step, 1.5)
})

test_that("We can write parcellation data", {
  template <- system.file("extdata", "cifti", "tiny.dlabel.nii", package = "freesurferformats")
  keys <- read.fs.parcellation.cifti(template, "lh")
  label_table <- read.fs.parcellation.cifti(template, "lh", with_label_table = TRUE)$label_table

  # The label table of the template is kept when none is given.
  out_file <- cifti.test.write.file("parc.dlabel.nii")
  write.fs.parcellation.cifti(out_file, keys, template = template, structure = "lh")
  read_back <- read.fs.parcellation.cifti(out_file, "lh", with_label_table = TRUE)
  expect_equal(as.integer(read_back$label_keys), as.integer(keys))
  expect_equal(read_back$label_table$Label, label_table$Label)
  expect_equal(read_back$map_name, "#1")

  # An explicit label table, and a map name.
  out_file <- cifti.test.write.file("parc2.dlabel.nii")
  write.fs.parcellation.cifti(out_file, keys, template = template, structure = "lh",
                              label_table = label_table, map_name = "aparc")
  read_back <- read.fs.parcellation.cifti(out_file, "lh", with_label_table = TRUE)
  expect_equal(read_back$map_name, "aparc")
  expect_equal(read_back$label_table$Key, label_table$Key)

  # The colortable of a FreeSurfer annotation can be used directly. Its colors are in the
  # range 0 to 255 and its codes are the label keys.
  annot <- read.fs.annot(system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats"))
  out_file <- cifti.test.write.file("parc_annot.dlabel.nii")
  write.fs.parcellation.cifti(out_file, annot$label_codes, structure = "lh", label_table = annot$colortable)
  read_back <- read.fs.parcellation.cifti(out_file, "lh", with_label_table = TRUE)
  expect_equal(as.integer(read_back$label_keys), as.integer(annot$label_codes))
  expect_equal(nrow(read_back$label_table), nrow(annot$colortable$table))
  expect_equal(read_back$label_table$Label[1], "unknown")
  expect_equal(round(read_back$label_table$Red[1] * 255), 25)

  expect_error(write.fs.parcellation.cifti(cifti.test.write.file("x.dlabel.nii"), keys, template = template,
                                           structure = "lh", label_table = "not a table"),
               "must be a data.frame")
})

test_that("Written files of the official example data match the originals", {
  testthat::skip_on_cran()
  dtseries_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"))
  dlabel_file <- find_extra_test_data_file(file.path("cifti", "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii"))
  testthat::skip_if(is.null(dtseries_file) || is.null(dlabel_file),
                    message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  # A dtseries file with the mapping of the official one: the axes, the data and the
  # metadata have to survive, and the values that are not in the file stay NA.
  out_file <- cifti.test.write.file("official.dtseries.nii")
  original <- read.cifti(dtseries_file)
  write.cifti(out_file, original$data, template = dtseries_file)
  rewritten <- read.cifti(out_file)
  expect_equal(dim(rewritten$data), dim(original$data))
  expect_equal(unname(rewritten$data[, 1:100]), unname(original$data[, 1:100]))
  expect_equal(rewritten$header$matrix$indices_maps, original$header$matrix$indices_maps)
  expect_equal(rewritten$header$matrix$metadata, original$header$matrix$metadata)
  expect_equal(read.fs.morph.cifti(out_file, "lh"), read.fs.morph.cifti(dtseries_file, "lh"))
  unlink(out_file)

  # The label file keeps its 3 maps with 96 labels each.
  out_file <- cifti.test.write.file("official.dlabel.nii")
  write.cifti(out_file, read.cifti(dlabel_file)$data, template = dlabel_file)
  rewritten <- read.cifti(out_file)
  expect_equal(dim(rewritten$data), dim(read.cifti(dlabel_file)$data))
  expect_equal(cifti.label.table(rewritten$header, 0L, 1L)$label, cifti.label.table(read.cifti.header(dlabel_file), 0L, 1L)$label)
  unlink(out_file)

  # The per-vertex morphometry data of the official file can be written back.
  morph_lh <- read.fs.morph.cifti(dtseries_file, "lh", 2L)
  out_file <- cifti.test.write_file <- cifti.test.write.file("official_morph.dscalar.nii")
  expect_warning(write.fs.morph.cifti(out_file, morph_lh, template = dtseries_file, structure = "lh"),
                 NA) # the template has a series dimension, the data have one map, which is fine
  expect_equal(dim(read.cifti(out_file)$data), c(1L, 30424L))
  expect_equal(as.numeric(read.fs.morph.cifti(out_file, "lh")), morph_lh)
  unlink(out_file)
})
