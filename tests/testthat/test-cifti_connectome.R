# Tests for the CIFTI-2 connectome reader and the writers for connectomes and parcellated
# files (R/read_cifti_connectome.R, R/write_cifti_connectome.R).
#
# The fixtures in inst/extdata/cifti cover all four connectome file types (`dconn`, `pconn`
# and the mixed `pdconn`/`dpconn`) plus the parcellated files. The written files are
# additionally verified against nibabel and Connectome Workbench by
# dev_tools/check_cifti_conversion.R, which is not part of the test suite because it needs
# those tools installed.

cifti.test.fixture <- function(filename) {
  return(system.file("extdata", "cifti", filename, package = "freesurferformats"))
}

cifti.test.out.file <- function(filename) {
  return(file.path(tempdir(), filename))
}

test_that("The connectome reader interprets a parcellated connectome", {
  conn <- read.fs.connectome.cifti(cifti.test.fixture("tiny.pconn.nii"))
  expect_s3_class(conn, "fs.connectome")
  expect_s3_class(conn$header, "fs.cifti")
  expect_equal(dim(conn$data), c(3L, 3L))
  expect_equal(conn$parcel_names, c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(rownames(conn$data), conn$parcel_names)
  expect_equal(colnames(conn$data), conn$parcel_names)
  expect_equal(conn$parcels_dim, 0L)
  expect_equal(as.character(conn$parcels$name), conn$parcel_names)
  expect_equal(conn$parcels$num_vertices, c(6L, 6L, 7L))
  # Both values of a connectome are stored in the file, also for a symmetric matrix.
  expect_equal(sum(conn$data != 1), 0L)
  expect_null(conn$grayordinates)
  expect_true(is.na(conn$grayordinates_dim))
})

test_that("The connectome reader interprets a dense connectome", {
  conn <- read.fs.connectome.cifti(cifti.test.fixture("tiny.dconn.nii"))
  expect_equal(dim(conn$data), c(22L, 22L))
  expect_null(conn$parcel_names)
  expect_null(conn$parcels)
  expect_true(is.na(conn$parcels_dim))
  expect_equal(conn$grayordinates_dim, 0L)
  expect_equal(nrow(conn$grayordinates), 22L)
  expect_true(all(conn$grayordinates$model_type == "SURFACE"))
  expect_equal(rownames(conn$data)[1L], "CORTEX_LEFT_0")
  expect_equal(colnames(conn$data)[11L], "CORTEX_RIGHT_0")
})

test_that("The connectome reader handles the mixed file types", {
  pdconn <- read.fs.connectome.cifti(cifti.test.fixture("tiny.pdconn.nii"))
  expect_equal(dim(pdconn$data), c(3L, 22L))
  expect_equal(pdconn$parcels_dim, 0L)
  expect_equal(pdconn$grayordinates_dim, 1L)
  expect_equal(pdconn$parcel_names, c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(nrow(pdconn$grayordinates), 22L)

  dpconn <- read.fs.connectome.cifti(cifti.test.fixture("tiny.dpconn.nii"))
  expect_equal(dim(dpconn$data), c(22L, 3L))
  expect_equal(dpconn$parcels_dim, 1L)
  expect_equal(dpconn$grayordinates_dim, 0L)
})

test_that("The connectome reader supports row and column selection", {
  conn_file <- cifti.test.fixture("tiny.dconn.nii")
  conn <- read.fs.connectome.cifti(conn_file, rows = 2:4, columns = c(3L, 1L))
  expect_equal(dim(conn$data), c(3L, 2L))
  all_labels <- cifti.dim.labels(read.cifti.header(conn_file), 0L)
  expect_equal(rownames(conn$data), all_labels[2:4])
  expect_equal(colnames(conn$data), all_labels[c(3L, 1L)])
  # The mapping describes all indices, also when only a part of the data was read.
  expect_equal(nrow(conn$grayordinates), 22L)
  expect_error(read.fs.connectome.cifti(conn_file, rows = 23L), "in range 1 to 22")
})

test_that("The connectome reader rejects files that are not connectomes", {
  expect_error(read.fs.connectome.cifti(cifti.test.fixture("tiny.dscalar.nii")), "not a connectome")
  expect_error(read.fs.connectome.cifti(cifti.test.fixture("tiny.dtseries.nii")), "not a connectome")
  expect_error(read.fs.connectome.cifti(cifti.test.fixture("tiny.pscalar.nii")), "not a connectome")
  expect_error(read.fs.connectome.cifti(cifti.test.fixture("tiny.dlabel.nii")), "not a connectome")
})

test_that("The connectome reader accepts header and data objects", {
  conn_file <- cifti.test.fixture("tiny.pconn.nii")
  expected <- read.fs.connectome.cifti(conn_file)
  from_header <- read.fs.connectome.cifti(read.cifti.header(conn_file))
  from_data <- read.fs.connectome.cifti(read.cifti(conn_file))
  expect_equal(from_header$data, expected$data)
  expect_equal(from_data$data, expected$data)
  expect_equal(from_data$parcel_names, expected$parcel_names)
  expect_error(read.fs.connectome.cifti(read.cifti(conn_file), columns = 1L), "only be used when reading from a file")
  expect_error(read.fs.connectome.cifti(42L), "must be the path of a CIFTI-2 file")
})

test_that("The connectome print method summarizes the file", {
  conn <- read.fs.connectome.cifti(cifti.test.fixture("tiny.pconn.nii"))
  expect_output(print(conn), "Connectome matrix: 3 x 3 values")
  expect_output(print(conn), "Parcels \\(matrix dimension 0\\): 3, first ones: PARCEL_A, PARCEL_B, PARCEL_C")
  expect_invisible(print(conn))

  dense <- read.fs.connectome.cifti(cifti.test.fixture("tiny.dconn.nii"))
  expect_output(print(dense), "Brainordinates \\(matrix dimension 0\\): 22 in total, 22 of them surface vertices")
})

test_that("A parcellated connectome round trips through the writer", {
  conn_file <- cifti.test.fixture("tiny.pconn.nii")
  conn <- read.fs.connectome.cifti(conn_file)
  out_file <- cifti.test.out.file("written.pconn.nii")
  axes <- write.fs.connectome.cifti(out_file, conn$data^2, template = conn_file)
  expect_equal(cifti.axis.size(axes[[1L]]), 3L)
  expect_equal(cifti.axis.size(axes[[2L]]), 3L)

  back <- read.fs.connectome.cifti(out_file)
  expect_equal(unname(back$data), unname(conn$data^2))
  expect_equal(back$parcel_names, conn$parcel_names)
  expect_equal(back$parcels$num_vertices, conn$parcels$num_vertices)
  header <- read.cifti.header(out_file)
  expect_equal(header$niiheader$intent_code, 3003L)
  # The axes of the two dimensions are identical, so they are written as one mapping.
  expect_length(header$matrix$indices_maps, 1L)
  expect_equal(header$matrix$indices_maps[[1L]]$dims, c(0L, 1L))
  # The metadata of the template is kept.
  expect_equal(length(header$matrix$metadata), length(conn$header$matrix$metadata))
  unlink(out_file)
})

test_that("A dense connectome can be written with the mapping of a dense scalar file", {
  template <- cifti.test.fixture("tiny.dscalar.nii")
  out_file <- cifti.test.out.file("written.dconn.nii")
  data <- matrix(as.numeric(seq_len(22L * 22L)), nrow = 22L)
  # The dscalar has a single brainordinate dimension, and its mapping is used for both.
  expect_message(write.fs.connectome.cifti(out_file, data, template = template),
                 "one brainordinate dimension")

  back <- read.fs.connectome.cifti(out_file)
  expect_equal(dim(back$data), c(22L, 22L))
  expect_equal(unname(back$data), data)
  expect_equal(nrow(back$grayordinates), 22L)
  expect_equal(back$grayordinates$vertex_index,
               cifti.grayordinates(read.cifti.header(template), 1L)$vertex_index)
  header <- read.cifti.header(out_file)
  expect_equal(header$niiheader$intent_code, 3001L)
  expect_equal(cifti.map.for.dim(header, 0L)$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_equal(cifti.map.for.dim(header, 1L)$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  unlink(out_file)
})

test_that("A connectome can be written from explicit axes", {
  data <- matrix(as.numeric(seq_len(22L * 22L)), nrow = 22L)
  axis <- cifti.axis.from.template(cifti.test.fixture("tiny.dconn.nii"), dim = 0L)
  out_file <- cifti.test.out.file("written_axes.dconn.nii")
  expect_message(write.fs.connectome.cifti(out_file, data, axes = axis), "same axis for both")
  expect_equal(unname(read.cifti(out_file)$data), data)

  # Two axes are used as they are, without a message.
  out_two <- cifti.test.out.file("written_two.dconn.nii")
  expect_silent(write.fs.connectome.cifti(out_two, data, axes = cifti.axis.from.template(out_file)))
  expect_equal(unname(read.cifti(out_two)$data), data)
  unlink(out_file)
  unlink(out_two)
})

test_that("A connectome can be written from an fs.connectome instance", {
  conn_file <- cifti.test.fixture("tiny.pconn.nii")
  conn <- read.fs.connectome.cifti(conn_file)
  out_file <- cifti.test.out.file("written_from_object.pconn.nii")
  write.fs.connectome.cifti(out_file, conn)
  expect_equal(unname(read.fs.connectome.cifti(out_file)$data), unname(conn$data))
  unlink(out_file)
})

test_that("The mixed connectome types can be written with the axes of a template", {
  for (fixture in c("tiny.pdconn.nii", "tiny.dpconn.nii")) {
    template <- cifti.test.fixture(fixture)
    out_file <- cifti.test.out.file(paste0("written_", fixture))
    expect_silent(write.fs.connectome.cifti(out_file, read.cifti(template)$data, template = template))
    back <- read.cifti(out_file)
    expect_equal(unname(back$data), unname(read.cifti(template)$data))
    expect_equal(back$header$niiheader$intent_code, read.cifti.header(template)$niiheader$intent_code)
    unlink(out_file)
  }
})

test_that("The connectome writer validates the data and the file type", {
  dconn_axis <- cifti.axis.from.template(cifti.test.fixture("tiny.dconn.nii"), dim = 0L)
  data <- matrix(as.numeric(seq_len(22L * 22L)), nrow = 22L)

  out_file <- cifti.test.out.file("invalid.dconn.nii")
  expect_error(write.fs.connectome.cifti(out_file, data), "Either 'template' or 'axes' has to be given")
  expect_error(write.fs.connectome.cifti(out_file, as.character(data), axes = dconn_axis), "must be a numeric or integer matrix")
  expect_error(write.fs.connectome.cifti(out_file, as.numeric(seq_len(10L)), axes = dconn_axis), "must be a numeric or integer matrix")
  expect_error(write.fs.connectome.cifti(out_file, matrix(1, nrow = 3L, ncol = 22L), axes = dconn_axis),
               "has the dimensions 3 x 22")
  # A transposed matrix is reported with a hint.
  pdconn_axis <- cifti.axis.from.template(cifti.test.fixture("tiny.pdconn.nii"))
  expect_error(write.fs.connectome.cifti(cifti.test.out.file("tp.pdconn.nii"), matrix(1, nrow = 22L, ncol = 3L),
                                         axes = pdconn_axis),
               "look transposed")
  # The file name has to agree with the axes.
  expect_error(write.fs.connectome.cifti(cifti.test.out.file("mismatch.pdconn.nii"), data, axes = dconn_axis),
               "names the CIFTI-2 file type '.pdconn.nii'")
  # The axes have to describe brainordinates or parcels.
  scalar_axis <- cifti.axis.scalars(c("a", "b"))
  expect_error(write.fs.connectome.cifti(out_file, matrix(1, nrow = 2L, ncol = 2L), axes = scalar_axis),
               "a connectome dimension holds brainordinates or parcels")
  expect_error(write.fs.connectome.cifti(out_file, matrix(1, nrow = 2L, ncol = 2L),
                                         axes = list(scalar_axis, scalar_axis, scalar_axis)),
               "at most two matrix dimensions")
  # A template without a brainordinate or parcel dimension cannot define the mapping.
  no_conn_file <- cifti.test.out.file("no_connectome.nii")
  expect_warning(write.cifti(no_conn_file, matrix(1, nrow = 2L, ncol = 2L),
                             axes = list(cifti.axis.scalars(c("a", "b")), cifti.axis.series(2L))),
                 "not one of the nine standard")
  expect_error(write.fs.connectome.cifti(out_file, matrix(1, nrow = 2L, ncol = 2L), template = no_conn_file),
               "has no brainordinate or parcel dimension")
  unlink(no_conn_file)
})

test_that("A parcellated series round trips through the writer", {
  template <- cifti.test.fixture("tiny.ptseries.nii")
  data <- matrix(as.numeric(seq_len(4L * 3L)), nrow = 4L)
  out_file <- cifti.test.out.file("written.ptseries.nii")
  write.fs.parcellated.cifti(out_file, data, template = template, start = 2, step = 0.5)

  back <- read.cifti(out_file)
  expect_equal(unname(back$data), data)
  expect_equal(dim(back$data), c(4L, 3L))
  expect_equal(cifti.series.info(back$header, 0L), list(number_of_series_points = 4L, start = 2, step = 0.5,
                                                         exponent = 0L, unit = "SECOND"))
  expect_equal(as.character(cifti.parcels(back$header, 1L)$name), c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  expect_equal(back$header$niiheader$intent_code, 3004L)
  expect_equal(dimnames(back$data)[[2L]], c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  unlink(out_file)
})

test_that("A parcellated scalar map round trips through the writer", {
  template <- cifti.test.fixture("tiny.pscalar.nii")

  # A vector is one value per parcel, i.e. a file with a single map.
  out_file <- cifti.test.out.file("written.pscalar.nii")
  write.fs.parcellated.cifti(out_file, c(1.5, 2.5, 3.5), template = template, map_names = "mean")
  back <- read.cifti(out_file)
  expect_equal(dim(back$data), c(1L, 3L))
  expect_equal(as.numeric(back$data[1L, ]), c(1.5, 2.5, 3.5))
  expect_equal(cifti.dim.labels(back$header, 0L), "mean")
  expect_equal(back$header$niiheader$intent_code, 3008L)
  unlink(out_file)

  # A matrix is one row per map, and the map names of a pscalar template are kept.
  maps <- matrix(as.numeric(seq_len(4L * 3L)), nrow = 4L)
  out_maps <- cifti.test.out.file("written_maps.pscalar.nii")
  write.fs.parcellated.cifti(out_maps, maps, template = template)
  back_maps <- read.cifti(out_maps)
  expect_equal(unname(back_maps$data), maps)
  expect_equal(cifti.dim.labels(back_maps$header, 0L), paste0("lhmap", 1:4))
  unlink(out_maps)
})

test_that("A parcellated file can be written from a parcels axis instead of a template", {
  parcels_axis <- cifti.axis.from.template(cifti.test.fixture("tiny.pconn.nii"), dim = 0L)
  out_file <- cifti.test.out.file("written_axis.pscalar.nii")
  write.fs.parcellated.cifti(out_file, c(1, 2, 3), axes = parcels_axis, map_names = "value")
  back <- read.cifti(out_file)
  expect_equal(as.numeric(back$data[1L, ]), c(1, 2, 3))
  expect_equal(as.character(cifti.parcels(back$header, 1L)$name), c("PARCEL_A", "PARCEL_B", "PARCEL_C"))
  unlink(out_file)

  # The same axis twice makes a parcellated connectome.
  out_conn <- cifti.test.out.file("written_axis.pconn.nii")
  write.fs.connectome.cifti(out_conn, matrix(1, nrow = 3L, ncol = 3L), axes = parcels_axis)
  expect_equal(read.cifti.header(out_conn)$niiheader$intent_code, 3003L)
  unlink(out_conn)
})

test_that("The parcellated writer validates the data, the axes and the file type", {
  template <- cifti.test.fixture("tiny.ptseries.nii")
  parcels_axis <- cifti.axis.from.template(template, dim = 1L)
  data <- matrix(as.numeric(seq_len(4L * 3L)), nrow = 4L)

  out_file <- cifti.test.out.file("invalid.ptseries.nii")
  expect_error(write.fs.parcellated.cifti(cifti.test.out.file("invalid.dscalar.nii"), data, template = template),
               "writes the parcellated file types")
  expect_error(write.fs.parcellated.cifti(out_file, data), "Either 'template' or 'axes' has to be given")
  expect_error(write.fs.parcellated.cifti(out_file, data, template = cifti.test.fixture("tiny.dscalar.nii")),
               "has no parcels")
  expect_error(write.fs.parcellated.cifti(out_file, c(1, 2), template = template), "have 2 values")
  expect_error(write.fs.parcellated.cifti(out_file, matrix(1, nrow = 3L, ncol = 4L), template = template),
               "look transposed")
  expect_error(write.fs.parcellated.cifti(out_file, as.character(data), template = template), "must be numeric")
  expect_error(write.fs.parcellated.cifti(out_file, data, axes = cifti.axis.series(4L)), "type 'SERIES'")
  expect_error(write.fs.parcellated.cifti(out_file, data, axes = list(cifti.axis.series(4L), cifti.axis.series(4L))),
               "Use a parcels axis")
  expect_error(write.fs.parcellated.cifti(out_file, data,
                                          axes = list(cifti.axis.series(4L), parcels_axis, parcels_axis)),
               "at most two matrix dimensions")
  expect_error(write.fs.parcellated.cifti(out_file, data,
                                          axes = list(cifti.axis.series(2L), parcels_axis)),
               "axes describe a matrix of size 2 x 3")
})
