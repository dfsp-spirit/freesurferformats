test_that("Header extensions of NIFTI v2 files can be read.", {
  # The files are written with write.nifti2 in these tests, and read back with the NIFTI v2 reader, so this
  # covers both sides. A real-world file written by Connectome Workbench is read further below.
  niidata <- array(as.double(1:24), dim = c(4, 3, 2))
  ext_comment <- nifti2.extension(6L, "a comment stored in a header extension")
  ext_raw <- nifti2.extension(4L, as.raw(1:7))

  nii_file <- tempfile(fileext = ".nii")
  write.nifti2(nii_file, niidata, extensions = list(ext_comment, ext_raw))

  hdr <- read.nifti2.header(nii_file)
  testthat::expect_equal(length(hdr$extensions), 2L)
  testthat::expect_equal(hdr$extensions[[1L]]$ecode, 6L)
  testthat::expect_equal(hdr$extensions[[2L]]$ecode, 4L)
  testthat::expect_equal(hdr$extensions[[1L]]$content, ext_comment$content)
  testthat::expect_equal(hdr$extensions[[2L]]$content, ext_raw$content)

  # The content is raw data, and can be converted to text for text payloads.
  testthat::expect_equal(nifti2.extension.text(hdr$extensions[[1L]]), "a comment stored in a header extension")

  # The extension can be retrieved by code.
  testthat::expect_equal(nifti2.get.extension(hdr, 6L)$content, ext_comment$content)
  testthat::expect_true(is.null(nifti2.get.extension(hdr, 32L))) # no extension with this code in the file.
  testthat::expect_true(is.null(nifti2.get.extension(list(), 32L))) # no extensions at all.
  testthat::expect_true(is.null(nifti2.get.extension(ni2header.template(), 32L)))

  # The data is not affected by the extensions.
  testthat::expect_equal(read.nifti2.data(nii_file, header = hdr), niidata)

  # Files without extensions report an empty extension list.
  nii_file_no_ext <- tempfile(fileext = ".nii")
  write.nifti2(nii_file_no_ext, niidata)
  hdr_no_ext <- read.nifti2.header(nii_file_no_ext)
  testthat::expect_equal(length(hdr_no_ext$extensions), 0L)
  testthat::expect_true(is.null(nifti2.get.extension(hdr_no_ext, 32L)))

  # Compressed NIFTI v2 files with extensions work as well.
  nii_file_gz <- tempfile(fileext = ".nii.gz")
  write.nifti2(nii_file_gz, niidata, extensions = list(ext_comment))
  hdr_gz <- read.nifti2.header(nii_file_gz)
  testthat::expect_equal(length(hdr_gz$extensions), 1L)
  testthat::expect_equal(nifti2.extension.text(hdr_gz$extensions[[1L]]), "a comment stored in a header extension")
  testthat::expect_equal(read.nifti2.data(nii_file_gz, header = hdr_gz), niidata)
})


test_that("Invalid NIFTI v2 header extensions are reported and do not crash the reader.", {
  niidata <- array(as.double(1:24), dim = c(4, 3, 2))
  nii_file <- tempfile(fileext = ".nii")
  write.nifti2(nii_file, niidata, extensions = list(nifti2.extension(6L, "some text payload")))

  file_bytes <- readBin(nii_file, "raw", n = file.size(nii_file))

  # The size field of the first extension is the 4 bytes at file offsets 544 to 547 (0-based).
  set_extension_size <- function(rawdata, size) {
    rawdata[545:548] <- writeBin(as.integer(size), raw(), size = 4L, endian = "little")
    return(rawdata)
  }

  # A size that is smaller than the 8 bytes of the extension meta data is invalid.
  broken_file <- tempfile(fileext = ".nii")
  writeBin(set_extension_size(file_bytes, 3L), broken_file)
  testthat::expect_warning(hdr <- read.nifti2.header(broken_file), "smaller than the 16 bytes")
  testthat::expect_equal(length(hdr$extensions), 0L)

  # So is a size that claims more bytes than the file has before the data.
  broken_file2 <- tempfile(fileext = ".nii")
  writeBin(set_extension_size(file_bytes, 4096L), broken_file2)
  testthat::expect_warning(hdr2 <- read.nifti2.header(broken_file2), "larger than")
  testthat::expect_equal(length(hdr2$extensions), 0L)

  # A size that is not a multiple of 16 bytes violates the standard, but the extension is read anyway.
  broken_file3 <- tempfile(fileext = ".nii")
  writeBin(set_extension_size(file_bytes, 24L), broken_file3)
  testthat::expect_warning(hdr3 <- read.nifti2.header(broken_file3), "not a multiple of 16")
  testthat::expect_equal(length(hdr3$extensions), 1L)
})


test_that("NIFTI v2 files with the NIFTI v1 magic are reported.", {
  niidata <- array(as.double(1:24), dim = c(4, 3, 2))
  nii_file <- tempfile(fileext = ".nii")
  write.nifti2(nii_file, niidata)

  file_bytes <- readBin(nii_file, "raw", n = file.size(nii_file))
  # Replace the magic 'n+2\0\r\n\032\n' (bytes 4 to 11, 0-based) by the NIFTI v1 magic 'n+1' plus zeroes,
  # which is what versions of this package before 1.1.0 wrote for NIFTI v2 files.
  file_bytes[5:12] <- as.raw(c(0x6e, 0x2b, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00))
  legacy_file <- tempfile(fileext = ".nii")
  writeBin(file_bytes, legacy_file)

  testthat::expect_warning(hdr <- read.nifti2.header(legacy_file), "magic string of this NIFTI v2 file is 'n\\+1'")
  # The file is still read, so that old files remain accessible.
  testthat::expect_equal(read.nifti2.data(legacy_file, header = hdr), niidata)
})


test_that("The header extension of a CIFTI2 file written by Connectome Workbench can be read.", {
  cifti_file <- find_extra_test_data_file(file.path("cifti", "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii"))
  testthat::skip_if(is.null(cifti_file), message = "Test data missing.")

  hdr <- read.nifti2.header(cifti_file)
  testthat::expect_equal(length(hdr$extensions), 1L)
  testthat::expect_equal(hdr$extensions[[1L]]$ecode, 32L)

  # CIFTI2 stores its XML metadata in an extension with code 32.
  cifti_ext <- nifti2.get.extension(hdr, 32L)
  testthat::expect_false(is.null(cifti_ext))
  cifti_xml <- nifti2.extension.text(cifti_ext)
  testthat::expect_true(grepl("CIFTI", cifti_xml, fixed = TRUE))
  testthat::expect_true(grepl("MatrixIndicesMap", cifti_xml, fixed = TRUE))

  # The XML can be parsed, i.e., the NUL padding has been removed correctly.
  testthat::expect_s3_class(xml2::read_xml(nifti2.extension.content(cifti_ext, strip_nul = FALSE)), "xml_document")

  # The data offset is behind the extension, and the data can be read.
  testthat::expect_true(hdr$vox_offset > 544L)
  testthat::expect_equal(length(read.nifti2.data(cifti_file, header = hdr)), 2L * 54L)
})
