# Tests for the memory-efficient row reader of CIFTI-2 files (read.cifti.rows(), item I.2f).
#
# The reader exists for files that do not fit into memory (the matrix of an HCP subject
# has 91,282 x 91,282 values, 33 GB), so the tests have to prove three things without
# shipping a large file: the values are exactly the ones of the full reader, the result
# does not depend on the chunk size (which is the only knob that affects the memory usage),
# and the reader never consults the allocation guard - the whole point is that it can read
# from a file whose complete matrix would be refused.

cifti.test.fixture <- function(filename) {
  return(system.file("extdata", "cifti", filename, package = "freesurferformats"))
}

# A CIFTI-2 file whose header and XML describe a large matrix (5000 x 5000 values, 100 MB)
# while its payload holds only the values of one column (5000 values, 20 KB), i.e. a
# truncated file of the kind that makes the allocation guard necessary. It is built with
# the CIFTI-2 writer, so the header and the metadata are consistent with each other and
# only the data are missing.
cifti.test.file.with.huge.dims <- function(filepath, n = 5000L) {
  axes <- list(cifti.axis.series(n, start = 0, step = 1),
               cifti.axis.brain.models(list(cifti.brain.model.surface("lh", n))))
  file_type <- cifti.file.type.for.axes(axes)
  niiheader <- cifti.nifti.header.for.axes(axes, intent_code = file_type$intent_code,
                                           intent_name = file_type$intent_name)
  write.nifti2(filepath, matrix(as.numeric(seq_len(n)), nrow = 10L), niiheader,
               extensions = list(nifti2.extension(CIFTI_EXTENSION_CODE, cifti.header.from.axes(axes))))
  return(filepath)
}

test_that("The row reader returns the same values as the full reader", {
  fixture <- cifti.test.fixture("tiny.dtseries.nii")
  full <- read.cifti(fixture)$data

  # All rows of the file, in a different order and with duplicates.
  selected <- c(4L, 1L, 2L)
  part <- read.cifti.rows(fixture, rows = selected)
  expect_equal(unname(part$data), unname(full[selected, , drop = FALSE]))
  expect_equal(dim(part$data), c(3L, 22L))
  expect_s3_class(part, "fs.cifti.data")
  expect_s3_class(part$header, "fs.cifti")
  # The dimensions of the result are named like the ones of the full matrix.
  expect_equal(rownames(part$data), rownames(full)[selected])
  expect_equal(colnames(part$data), colnames(full))
  expect_equal(part$header$matrix$dim_sizes, read.cifti.header(fixture)$matrix$dim_sizes)

  # A single row.
  one_row <- read.cifti.rows(fixture, rows = 3L)
  expect_equal(dim(one_row$data), c(1L, 22L))
  expect_equal(as.numeric(one_row$data[1L, ]), as.numeric(full[3L, ]))
})

test_that("The row reader supports a column selection as well", {
  fixture <- cifti.test.fixture("tiny.dscalar.nii")
  full <- read.cifti(fixture)$data
  columns <- c(5L, 1L, 1L)
  part <- read.cifti.rows(fixture, rows = 2L, columns = columns)
  expect_equal(unname(part$data), unname(full[2L, columns, drop = FALSE]))
  expect_equal(colnames(part$data), colnames(full)[columns])
  expect_equal(rownames(part$data), rownames(full)[2L])

  # Every combination of a subset of the rows and the columns of every fixture.
  for (name in c("tiny.dscalar.nii", "tiny.dtseries.nii", "tiny.pconn.nii", "tiny.pdconn.nii",
                 "tiny.dpconn.nii", "tiny.dconn.nii", "tiny_roi.dscalar.nii")) {
    fixture <- cifti.test.fixture(name)
    full <- read.cifti(fixture)$data
    expected <- full[c(2L, 1L), c(3L, 1L), drop = FALSE]
    # Note: one of the files has less than 3 columns, so use valid indices only.
    rows <- c(min(2L, nrow(full)), 1L)
    cols <- c(min(3L, ncol(full)), 1L)
    expected <- full[rows, cols, drop = FALSE]
    part <- read.cifti.rows(fixture, rows = rows, columns = cols)
    expect_equal(unname(part$data), unname(expected), info = name)
    expect_equal(colnames(part$data), colnames(full)[cols], info = name)
    expect_equal(rownames(part$data), rownames(full)[rows], info = name)
  }
})

test_that("The result does not depend on the chunk size", {
  fixture <- cifti.test.fixture("tiny.dtseries.nii")
  full <- read.cifti(fixture)$data
  expected <- full[c(2L, 4L), , drop = FALSE]
  num_rows <- nrow(full)
  # A chunk that holds a single column, several columns, a fraction of the file and
  # (much) more than the file.
  for (chunk_values in c(num_rows, num_rows * 2L, num_rows * 3L + 1L, 1000L, 1000000L)) {
    part <- read.cifti.rows(fixture, rows = c(2L, 4L), chunk_values = chunk_values)
    expect_equal(unname(part$data), unname(expected), info = sprintf("chunk_values = %s", chunk_values))
  }
  # A chunk smaller than one column is refused, since it cannot hold a complete column.
  expect_error(read.cifti.rows(fixture, rows = 1L, chunk_values = num_rows - 1L), "at least the size of matrix dimension 0")
  expect_error(read.cifti.rows(fixture, rows = 1L, chunk_values = "many"), "single number")
})

test_that("The row reader validates its parameters", {
  fixture <- cifti.test.fixture("tiny.dtseries.nii")
  expect_error(read.cifti.rows(fixture), "Parameter 'rows' must be given")
  expect_error(read.cifti.rows(fixture, rows = NULL), "Parameter 'rows' must be given")
  expect_error(read.cifti.rows(fixture, rows = 5L), "in range 1 to 4")
  expect_error(read.cifti.rows(fixture, rows = 0L), "in range 1 to 4")
  expect_error(read.cifti.rows(fixture, rows = 1.5), "in range 1 to 4")
  expect_error(read.cifti.rows(fixture, rows = 1L, columns = 23L), "in range 1 to 22")
  expect_error(read.cifti.rows(42L, rows = 1L), "must be a character string")
  expect_error(read.cifti.rows(file.path(tempdir(), "missing.nii"), rows = 1L), "does not exist")
})

test_that("The row reader ignores the allocation guard", {
  fixture <- cifti.test.fixture("tiny.dtseries.nii")
  full <- read.cifti(fixture)$data
  old_limit <- getOption("freesurferformats.max_alloc_bytes", default = NULL)
  options(freesurferformats.max_alloc_bytes = 100) # bytes, i.e. far below anything
  on.exit({
    if (is.null(old_limit)) {
      options(freesurferformats.max_alloc_bytes = NULL)
    } else {
      options(freesurferformats.max_alloc_bytes = old_limit)
    }
  }, add = TRUE)

  # The complete matrix is refused, with a message that names both ways to read a part.
  expect_error(read.cifti(fixture), "exceeds the safety limit")
  expect_error(read.cifti(fixture), "'columns' parameter of read.cifti")
  expect_error(read.cifti(fixture), "read.cifti.rows\\(\\)")

  # The row reader reads a small selection anyway: it never holds the whole matrix.
  part <- read.cifti.rows(fixture, rows = 2L)
  expect_equal(dim(part$data), c(1L, 22L))
  expect_equal(as.numeric(part$data[1L, 1L:3L]), as.numeric(full[2L, 1L:3L]))
})

test_that("The row reader works on a file whose dimensions are far too large", {
  # A file whose header and XML describe a 5000 x 5000 matrix (100 MB) but whose payload
  # holds 100 values: the allocation guard looks at the header, and the row reader must
  # not be stopped by it. The read then fails because the file is truncated, i.e. the
  # reader reached the data instead of refusing the file.
  huge_file <- file.path(tempdir(), "huge_header.nii")
  cifti.test.file.with.huge.dims(huge_file, n = 5000L)
  expect_equal(read.cifti.header(huge_file)$matrix$dim_sizes, c(5000L, 5000L))
  expect_equal(cifti.structures(read.cifti.header(huge_file), 1L)$index_count, 5000L)

  old_limit <- getOption("freesurferformats.max_alloc_bytes", default = NULL)
  options(freesurferformats.max_alloc_bytes = 1e6)
  on.exit({
    if (is.null(old_limit)) {
      options(freesurferformats.max_alloc_bytes = NULL)
    } else {
      options(freesurferformats.max_alloc_bytes = old_limit)
    }
    unlink(huge_file)
  }, add = TRUE)

  expect_error(read.cifti(huge_file), "exceeds the safety limit")
  # The one column that the truncated payload contains can be read, together with the
  # requested rows - the reader reached the data instead of refusing the file.
  part <- read.cifti.rows(huge_file, rows = c(1L, 10L), columns = 1L)
  expect_equal(dim(part$data), c(2L, 1L))
  expect_equal(as.numeric(part$data[, 1L]), c(1, 10))
  # Reading rows without a column selection needs the whole file, which is not there.
  expect_error(read.cifti.rows(huge_file, rows = 1L, chunk_values = 5000L), "Truncated file")
})
