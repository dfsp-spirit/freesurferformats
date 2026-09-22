test_that("Header extensions of NIFTI v2 files can be written.", {
  niidata <- array(as.double(1:24), dim = c(4, 3, 2))
  ext_comment <- nifti2.extension(6L, "a comment stored in a header extension")
  ext_raw <- nifti2.extension(4L, as.raw(1:7))

  nii_file <- tempfile(fileext = ".nii")
  result <- write.nifti2(nii_file, niidata, extensions = list(ext_comment, ext_raw))

  # The extensions are returned, and the offset of the data was adapted to fit them.
  testthat::expect_equal(length(result$extensions), 2L)
  testthat::expect_equal(result$header$vox_offset, 544L + nifti2.extension.size(ext_comment) + nifti2.extension.size(ext_raw))

  file_bytes <- readBin(nii_file, "raw", n = file.size(nii_file))

  # The 4 bytes at offset 540 indicate that extensions follow (the first byte must be non-zero). The bytes are
  # at 0-based offset 540, i.e. at 1-based index 541 in the R vector.
  testthat::expect_equal(file_bytes[541:544], as.raw(c(1L, 0L, 0L, 0L)))

  # Each extension starts with its size (a multiple of 16 bytes) and its code, both 4 byte integers.
  testthat::expect_equal(readBin(file_bytes[545:548], integer(), n = 1L, size = 4L), nifti2.extension.size(ext_comment))
  testthat::expect_equal(readBin(file_bytes[549:552], integer(), n = 1L, size = 4L), 6L)

  # The payload starts at offset 552, is padded with NUL bytes to a multiple of 16 bytes in total.
  testthat::expect_equal(nifti2.extension.size(ext_comment) %% 16L, 0L)
  testthat::expect_equal(nifti2.extension.size(ext_raw) %% 16L, 0L)

  # A file written without extensions is unchanged (the flag bytes are zero, the data starts at 544).
  nii_file_no_ext <- tempfile(fileext = ".nii")
  result_no_ext <- write.nifti2(nii_file_no_ext, niidata)
  testthat::expect_equal(result_no_ext$header$vox_offset, 544L)
  testthat::expect_equal(length(result_no_ext$extensions), 0L)
  bytes_no_ext <- readBin(nii_file_no_ext, "raw", n = 12L)
  testthat::expect_equal(bytes_no_ext[5:12], as.raw(c(0x6e, 0x2b, 0x32, 0x00, 0x0d, 0x0a, 0x1a, 0x0a)))

  # The magic string of a NIFTI v2 file is the 8 bytes 'n+2\0\r\n\032\n' as required by the standard. Older
  # versions of this package wrote the NIFTI v1 magic here, and other software refuses such files.
  testthat::expect_equal(file_bytes[5:12], as.raw(c(0x6e, 0x2b, 0x32, 0x00, 0x0d, 0x0a, 0x1a, 0x0a)))
  testthat::expect_equal(as.character(ni2header.template()$magic), "n+2")

  # A header read from a file can be written back with its extensions, without passing them again.
  hdr <- read.nifti2.header(nii_file)
  nii_file2 <- tempfile(fileext = ".nii")
  write.nifti2(nii_file2, niidata, hdr)
  hdr2 <- read.nifti2.header(nii_file2)
  testthat::expect_equal(length(hdr2$extensions), 2L)
  testthat::expect_equal(hdr2$extensions[[1L]]$content, ext_comment$content)
  testthat::expect_equal(hdr2$extensions[[2L]]$content, ext_raw$content)
  testthat::expect_equal(hdr2$vox_offset, hdr$vox_offset)

  # An explicitly requested larger data offset is kept, and the gap is filled with zeroes.
  hdr_large <- ni2header.for.data(niidata)
  hdr_large$vox_offset <- 1024L
  nii_file3 <- tempfile(fileext = ".nii")
  write.nifti2(nii_file3, niidata, hdr_large, extensions = list(ext_raw))
  testthat::expect_equal(read.nifti2.header(nii_file3)$vox_offset, 1024L)
  testthat::expect_equal(file.size(nii_file3), 1024L + length(niidata) * 4L)
})


test_that("NIFTI v2 header extensions are constructed and validated correctly.", {
  # Text content is stored as UTF-8 bytes, raw content as-is, NULL means an empty payload.
  ext <- nifti2.extension(6L, "hello")
  testthat::expect_equal(ext$ecode, 6L)
  testthat::expect_true(is.raw(ext$content))
  testthat::expect_equal(nifti2.extension.text(ext), "hello")

  ext_empty <- nifti2.extension(6L)
  testthat::expect_equal(length(ext_empty$content), 0L)
  testthat::expect_equal(nifti2.extension.text(ext_empty), "")

  ext_unicode <- nifti2.extension(6L, "Bra\u0308in") # Umlaut, as UTF-8
  testthat::expect_equal(nifti2.extension.text(ext_unicode), "Bra\u0308in")

  # Several strings are joined with newlines, like a character vector of lines.
  ext_lines <- nifti2.extension(6L, c("line one", "line two"))
  testthat::expect_equal(nifti2.extension.text(ext_lines), "line one\nline two")

  # The size on disk is a multiple of 16 bytes and includes the 8 bytes of meta data. Padding is added
  # as needed, but no padding is used if the payload fits exactly.
  testthat::expect_equal(nifti2.extension.size(nifti2.extension(6L, raw(8L))), 16L) # 8 + 8 bytes, no padding.
  testthat::expect_equal(nifti2.extension.size(nifti2.extension(6L, raw(9L))), 32L) # 9 + 8 rounds up to 32.
  testthat::expect_equal(nifti2.extension.size(nifti2.extension(6L, raw(24L))), 32L) # 24 + 8 bytes, no padding.

  # Error handling.
  testthat::expect_error(nifti2.extension("not a code", "x")) # ecode must be numeric.
  testthat::expect_error(nifti2.extension(c(1L, 2L), "x")) # ecode must be a single value.
  testthat::expect_error(nifti2.extension(NA_integer_, "x")) # ecode must not be NA.
  testthat::expect_error(nifti2.extension(-1L, "x")) # ecode must not be negative.
  testthat::expect_error(nifti2.extension(6L, 42L)) # content must be raw, character or NULL.

  testthat::expect_error(nifti2.get.extension("not a header", 6L))
  testthat::expect_error(write.nifti2(tempfile(fileext = ".nii"), array(1:4), extensions = "not a list"))

  # Trailing NUL bytes are padding and are removed when reading a file, but NUL bytes inside the payload are
  # kept as long as they are not at the end.
  testthat::expect_equal(nifti2.trailing.nul.removed(as.raw(c(1L, 2L, 0L, 0L))), as.raw(c(1L, 2L)))
  testthat::expect_equal(nifti2.trailing.nul.removed(as.raw(c(1L, 0L, 2L))), as.raw(c(1L, 0L, 2L)))
  testthat::expect_equal(nifti2.trailing.nul.removed(as.raw(c(0L, 0L))), raw(0L))
  testthat::expect_equal(nifti2.strip.nul(as.raw(c(1L, 0L, 2L, 0L))), as.raw(c(1L, 2L)))
})
