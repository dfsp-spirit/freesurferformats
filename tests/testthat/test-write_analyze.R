# Tests for writing ANALYZE 7.5 files and NIFTI v1 pairs (see R/write_analyze.R and R/write_nifti1.R), and for the
# reading of the files that were written. The files written here are verified against nibabel and FreeSurfer in
# dev_tools/check_analyze_conversion.R, the tests below check the behavior of the functions themselves.


test_that("The ANALYZE header template is complete and valid", {
  header <- analyzeheader.template()

  expect_equal(header$sizeof_hdr, 348L)
  expect_equal(header$dim, c(3L, 256L, 256L, 256L, 1L, 1L, 1L, 1L))
  expect_equal(length(header$pix_dim), 8L)
  expect_equal(header$magic, "")
  expect_equal(header$header_format, "analyze")
  expect_null(header$spm_origin)
  expect_true(analyze.header.check(header))
})


test_that("The ANALYZE header for data sets the type, dimensions and value range", {
  int_header <- analyzeheader.for.data(array(1L:24L, dim = c(4L, 3L, 2L)))
  expect_equal(int_header$datatype, 8L) # signed 32 bit
  expect_equal(int_header$bitpix, 32L)
  expect_equal(int_header$dim, c(3L, 4L, 3L, 2L, 1L, 1L, 1L, 1L))
  expect_equal(int_header$cal_min, 1)
  expect_equal(int_header$cal_max, 24)

  double_header <- analyzeheader.for.data(array(as.double(1:24), dim = c(4L, 3L, 2L)))
  expect_equal(double_header$datatype, 16L) # float32
  expect_equal(double_header$bitpix, 32L)

  # A vector is treated as 1D data.
  expect_equal(analyzeheader.for.data(as.double(1:10))$dim, c(1L, 10L, 1L, 1L, 1L, 1L, 1L, 1L))

  expect_error(analyzeheader.for.data(as.character(1:3)), "integer or double")
  expect_error(analyzeheader.for.data(array(1:8, dim = rep(2L, 8L))), "at most 7 dimensions")
})


test_that("Data written with write.analyze can be read back", {
  outfile <- tempfile()

  # uint8 data with values above 127, to catch a reader that uses the wrong signedness.
  data <- array(as.integer(c(200L, 255L, 3L, 128L, 0L, 7L, 250L, 1L, 129L, 5L, 6L, 254L)), dim = c(4L, 3L))
  header <- analyzeheader.for.data(data)
  header$datatype <- 2L # unsigned 8 bit
  header$bitpix <- 8L
  header$pix_dim <- c(1., 1.5, 1.5, 2., 0., 0., 0., 0.)
  header$descrip <- "written by a unit test"
  write.analyze(outfile, data, header)

  expect_true(file.exists(paste0(outfile, ".hdr")))
  expect_true(file.exists(paste0(outfile, ".img")))
  expect_equal(file.size(paste0(outfile, ".hdr")), 348)
  expect_equal(file.size(paste0(outfile, ".img")), 12) # 12 values of 1 byte

  read_back <- read.analyze.data(outfile)
  expect_equal(as.integer(as.vector(read_back)), as.integer(as.vector(data)))
  read_header <- read.analyze.header(outfile)
  expect_equal(read_header$descrip, "written by a unit test")
  expect_equal(read_header$pix_dim[2:4], c(1.5, 1.5, 2.))
  expect_equal(read_header$datatype, 2L)

  # int16 data with negative values.
  data_i16 <- array(as.integer(c(-1L, 300L, -32768L, 32767L, 0L, 7L, 8L, 9L)), dim = c(4L, 2L))
  header_i16 <- analyzeheader.for.data(data_i16)
  header_i16$datatype <- 4L
  header_i16$bitpix <- 16L
  outfile_i16 <- tempfile()
  write.analyze(outfile_i16, data_i16, header_i16)
  expect_equal(as.integer(as.vector(read.analyze.data(outfile_i16))), as.integer(as.vector(data_i16)))

  # float32 data.
  data_f32 <- array(c(0.25, 1.5, -2.75, 1e6), dim = c(2L, 2L))
  header_f32 <- analyzeheader.for.data(data_f32)
  header_f32$datatype <- 16L
  header_f32$bitpix <- 32L
  outfile_f32 <- tempfile()
  write.analyze(outfile_f32, data_f32, header_f32)
  expect_equal(as.vector(read.analyze.data(outfile_f32)), as.vector(data_f32), tolerance = 1e-4)
})


test_that("4D and compressed ANALYZE files can be written", {
  data <- array(as.integer(1L:(4L * 3L * 2L * 3L)), dim = c(4L, 3L, 2L, 3L))
  header <- analyzeheader.for.data(data)

  outfile <- tempfile()
  write.analyze(outfile, data, header)
  read_back <- read.analyze.data(outfile, drop_empty_dims = FALSE)
  expect_equal(dim(read_back), c(4L, 3L, 2L, 3L))
  expect_equal(as.integer(as.vector(read_back)), as.integer(as.vector(data)))

  # Files may be compressed, but note that not every tool reads compressed pairs.
  gzfile <- paste0(tempfile(), ".hdr.gz")
  write.analyze(gzfile, data, header)
  expect_true(file.exists(gzfile))
  expect_true(file.exists(sub("hdr\\.gz$", "img.gz", gzfile)))
  expect_equal(as.integer(as.vector(read.analyze.data(gzfile))), as.integer(as.vector(data)))
})


test_that("The SPM fields of an ANALYZE header can be written and read back", {
  outfile <- tempfile()
  data <- array(as.integer(1L:24L), dim = c(4L, 3L, 2L))
  header <- analyzeheader.for.data(data)
  header$funused1 <- 0.25
  header$spm_origin <- c(2L, 2L, 2L)
  header$orient <- 3L
  write.analyze(outfile, data, header)

  read_header <- read.analyze.header(outfile)
  expect_equal(read_header$funused1, 0.25)
  expect_equal(read_header$spm_origin, c(2L, 2L, 2L))
  expect_equal(read_header$orient, 3L)
  expect_equal(as.integer(charToRaw(read_header$originator)), c(2L, 2L, 2L))

  # The SPM origin and the originator string share the same 10 bytes, so setting both is reported.
  header_both <- header
  header_both$originator <- "bert"
  expect_warning(write.analyze(tempfile(), data, header_both), "originator")
})


test_that("The file name of an ANALYZE file must describe a pair", {
  data <- array(as.integer(1L:24L), dim = c(4L, 3L, 2L))
  header <- analyzeheader.for.data(data)

  # A base name without an extension is fine, the extensions are added.
  base <- tempfile()
  write.analyze(base, data, header)
  expect_true(file.exists(paste0(base, ".hdr")))
  expect_true(file.exists(paste0(base, ".img")))

  # So are the two file names of the pair.
  hdrfile <- tempfile(fileext = ".hdr")
  write.analyze(hdrfile, data, header)
  expect_true(file.exists(sub("hdr$", "img", hdrfile)))
  write.analyze(tempfile(fileext = ".img"), data, header)

  # But a single file name with another extension is not.
  expect_error(write.analyze(tempfile(fileext = ".nii"), data, header), "two files")

  expect_error(write.analyze(tempfile(), data, data.frame()), "Invalid ANALYZE")
})


test_that("Invalid ANALYZE headers are rejected by the writer", {
  data <- array(as.integer(1L:24L), dim = c(4L, 3L, 2L))
  header <- analyzeheader.for.data(data)

  header_long_descrip <- header
  header_long_descrip$descrip <- paste(rep("x", 81L), collapse = "")
  expect_message(analyze.header.check(header_long_descrip), "descrip")
  expect_false(analyze.header.check(header_long_descrip))
  expect_error(write.analyze(tempfile(), data, header_long_descrip), "Invalid ANALYZE")

  header_bad_dim <- header
  header_bad_dim$dim <- c(3L, 4L, 3L)
  expect_false(analyze.header.check(header_bad_dim))

  header_bad_size <- header
  header_bad_size$sizeof_hdr <- 540L
  expect_false(analyze.header.check(header_bad_size))
})


test_that("The NIFTI v1 header template can describe a pair", {
  single <- ni1header.template()
  expect_equal(single$magic, "n+1")
  expect_equal(single$vox_offset, 352.)

  pair <- ni1header.template(pair = TRUE)
  expect_equal(pair$magic, "ni1")
  expect_equal(pair$vox_offset, 0.)

  # The data type and dimensions are set the same way for both.
  pair_data <- ni1header.for.data(array(as.integer(1L:24L), dim = c(4L, 3L, 2L)), pair = TRUE)
  expect_equal(pair_data$magic, "ni1")
  expect_equal(pair_data$datatype, 8L)
})


test_that("write.nifti1 writes a NIFTI v1 pair when the header magic is 'ni1'", {
  data <- array(as.integer(1L:24L), dim = c(4L, 3L, 2L))
  pair_header <- ni1header.for.data(data, pair = TRUE)
  pair_header$sform_code <- 1L
  pair_header$srow_x <- c(-1., 0., 0., 11.)
  pair_header$srow_y <- c(0., 0., 1., 22.)
  pair_header$srow_z <- c(0., -1., 0., 33.)

  outfile <- tempfile()
  result <- write.nifti1(outfile, data, pair_header)
  expect_true(file.exists(paste0(outfile, ".hdr")))
  expect_true(file.exists(paste0(outfile, ".img")))
  expect_equal(file.size(paste0(outfile, ".img")), 24L * 4L)
  expect_equal(result$header$vox_offset, 0.)

  # The file is a pair, so the volume reader computes its geometry from the sform.
  vol <- read.fs.volume(paste0(outfile, ".hdr"), with_header = TRUE)
  expect_equal(as.integer(as.vector(vol$data)), as.integer(as.vector(data)))
  expect_equal(vol$header$vox2ras_source, "sform")
  expect_equal(unname(vol$header$vox2ras_matrix),
               matrix(c(-1., 0., 0., 11., 0., 0., 1., 22., 0., -1., 0., 33., 0., 0., 0., 1.), nrow = 4L, byrow = TRUE))

  # The NIFTI reader of the package can read the data of a pair file, given the header file.
  expect_equal(as.integer(as.vector(read.nifti1.data(paste0(outfile, ".hdr")))), as.integer(as.vector(data)))
  # The data file alone has no header, so it cannot be read on its own.
  expect_error(read.nifti1.data(paste0(outfile, ".img")), "not in NIFTI 1 format")

  # A non-zero data offset is meaningless for a pair, so it is corrected with a warning.
  bad_offset <- pair_header
  bad_offset$vox_offset <- 352.
  expect_warning(write.nifti1(tempfile(), data, bad_offset), "vox_offset")
})


test_that("write.nifti1 refuses file names and magics that do not match", {
  data <- array(as.integer(1L:24L), dim = c(4L, 3L, 2L))

  # A pair cannot be written into a single file name.
  expect_error(write.nifti1(tempfile(fileext = ".nii"), data, ni1header.for.data(data, pair = TRUE)), "two files")
  # And a single file image cannot be written into a header file name.
  expect_error(write.nifti1(tempfile(fileext = ".hdr"), data, ni1header.for.data(data)), "single file")

  # The single file variant still works as before.
  single <- tempfile(fileext = ".nii")
  write.nifti1(single, data)
  expect_true(file.exists(single))
  expect_equal(file.size(single), 352 + 24 * 4)
  expect_equal(as.integer(as.vector(read.nifti1.data(single))), as.integer(as.vector(data)))
})


test_that("Unsigned NIFTI data types are read without changing the values", {
  # R reads integers as signed by default, so a reader has to pass the signedness explicitly. This was wrong for
  # the 8 and 16 bit unsigned types before, which silently turned values above the signed maximum into negatives.
  data_u8 <- array(as.integer(c(200L, 255L, 3L, 128L, 0L, 7L, 250L, 1L, 129L, 5L, 6L, 254L)), dim = c(4L, 3L))
  header_u8 <- ni1header.for.data(data_u8)
  header_u8$datatype <- 2L
  header_u8$bitpix <- 8L
  file_u8 <- tempfile(fileext = ".nii")
  write.nifti1(file_u8, data_u8, header_u8)
  expect_equal(as.integer(as.vector(read.nifti1.data(file_u8))), as.integer(as.vector(data_u8)))

  # 16 bit unsigned, as a pair (the type does not exist in ANALYZE 7.5).
  data_u16 <- array(as.integer(c(40000L, 65535L, 1L, 32768L, 0L, 7L)), dim = c(3L, 2L))
  header_u16 <- ni1header.for.data(data_u16, pair = TRUE)
  header_u16$datatype <- 512L
  header_u16$bitpix <- 16L
  file_u16 <- tempfile()
  write.nifti1(file_u16, data_u16, header_u16)
  expect_equal(as.integer(as.vector(read.nifti1.data(paste0(file_u16, ".hdr")))), as.integer(as.vector(data_u16)))
  # The volume reader handles both variants of the two-file format, and reading a NIFTI pair with the ANALYZE
  # reader alone is refused with a pointer to the NIFTI reader.
  expect_equal(as.integer(as.vector(read.fs.volume.analyze(paste0(file_u16, ".hdr")))), as.integer(as.vector(data_u16)))
  expect_error(read.analyze.data(paste0(file_u16, ".hdr")), "not an ANALYZE 7.5 file")
})


test_that("write.fs.volume writes a NIFTI pair for .hdr file names", {
  brain_image <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
  vol <- read.fs.mgh(brain_image, with_header = TRUE)

  outfile <- tempfile(fileext = ".hdr")
  write.fs.volume(outfile, vol)
  expect_true(file.exists(outfile))
  expect_true(file.exists(sub("hdr$", "img", outfile)))

  read_back <- read.fs.volume(outfile, with_header = TRUE)
  expect_equal(as.numeric(as.vector(read_back$data)), as.numeric(as.vector(vol$data)))
  expect_true(!is.null(read_back$header$vox2ras_matrix))

  # Without a known extension, the volume cannot be written.
  expect_error(write.fs.volume(tempfile(), vol), "Invalid file extension")

  # A volume whose MGH header contains no voxel size information cannot be written to NIFTI at all, and the
  # error message says so instead of reporting a missing field.
  no_ras_vol <- read.fs.mgh(system.file("extdata", "tiny.mgh", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  expect_error(write.fs.volume(tempfile(fileext = ".hdr"), no_ras_vol), "no voxel size information")
})
