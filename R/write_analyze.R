# Writer for the ANALYZE 7.5 image format. See R/read_analyze.R for an overview of the format and for the
# difference between ANALYZE 7.5 and NIFTI v1 pair files (which share the 348 byte header and are also handled
# by this function when the magic field is set to 'ni1' by the caller, see write.nifti1()).


#' @title Write data to a file in ANALYZE 7.5 format.
#'
#' @description Write an array or vector to the two files that make up an ANALYZE 7.5 image: a 348 byte header
#'   (`<base>.hdr`) and the raw voxel data in a separate file (`<base>.img`).
#'
#' @param filepath character string, the path of the output files. It must end with `.hdr` or `.img` (optionally
#'   followed by `.gz`), or contain no extension at all, in which case the two file names are derived from it by
#'   appending `.hdr` and `.img`. See \code{\link{analyze.pair.files}}.
#'
#' @param analyzedata array of numeric (integer or double) data, can have up to 7 dimensions. See
#'   \code{\link{analyzeheader.for.data}} for how the data type is chosen, and note that the data type and the
#'   number of bits per value in the header have to match the data, otherwise the file is not readable.
#'
#' @param analyzeheader optional ANALYZE 7.5 header that is suitable for the passed `analyzedata`, see
#'   \code{\link{analyzeheader.for.data}} and \code{\link{analyzeheader.template}}. If not given, one is generated
#'   from the data, with all other fields set to the defaults of the format, i.e. the voxel sizes in `pix_dim` will
#'   be zero and you most likely want to set them.
#'
#' @param ... additional parameters passed to \code{\link{analyzeheader.for.data}}. Only used if `analyzeheader` is
#'   `NULL`.
#'
#' @return invisible named list with the entries `header` (the header that was written) and `data` (the data that
#'   was written). Use these to check what the function did with your input, they reflect the values that ended up in
#'   the file, e.g. values that had to be converted to the data type stated in the header.
#'
#' @note ANALYZE 7.5 cannot store a transformation matrix, and the format does not even define the direction of the
#'   voxel axes in world space. This writer therefore cannot write the geometry of an image: if you have a volume
#'   with a known `vox2ras_matrix` and you want to keep it, write a NIFTI v1 file with
#'   \code{\link{write.nifti1}} instead (either a single file, or a `.hdr`/`.img` pair by setting the header magic
#'   to `'ni1'`, which is what \code{\link{write.fs.volume}} does for such file names). If you know that the data
#'   will be read by SPM or FreeSurfer, you can store the image origin in the `spm_origin` field of the header, see
#'   \code{\link{read.analyze.header}}.
#'
#' @note Note that tools differ in how they recognize the format. FreeSurfer's `mri_convert`, for example, does not
#'   infer the ANALYZE format from the `.hdr` file extension and reports 'cannot determine file type' unless the
#'   input type is given explicitly: `mri_convert -it analyze -ot mgz vol.hdr vol.mgz` works, `mri_convert -ot mgz
#'   vol.hdr vol.mgz` does not. The header written by this function is the one FreeSurfer itself writes for
#'   `-ot analyze`, so this is a limitation of the reader, not of the file.
#'
#' @seealso \code{\link{read.analyze.header}}, \code{\link{analyzeheader.for.data}}
#'
#' @examples
#' outfiles <- tempfile()
#' data <- array(1:24, dim = c(4, 3, 2))
#' analyzeheader <- analyzeheader.for.data(data)
#' analyzeheader$pix_dim <- c(1., 1.5, 1.5, 2., 0., 0., 0., 0.)
#' write.analyze(outfiles, data, analyzeheader)
#' vol <- read.fs.volume.analyze(outfiles, with_header = TRUE)
#' vol$header$pix_dim
#'
#' @export
write.analyze <- function(filepath, analyzedata, analyzeheader = NULL, ...) {
  if (is.null(analyzeheader)) {
    analyzeheader <- analyzeheader.for.data(analyzedata, ...)
  }

  if (!analyze.header.check(analyzeheader)) {
    stop("Invalid ANALYZE 7.5 header.")
  }

  if (!filepath.ends.with(filepath, c(".hdr", ".img", ".hdr.gz", ".img.gz"))) {
    if (grepl(".", basename(filepath), fixed = TRUE)) {
      stop(sprintf("Invalid file name '%s' for an ANALYZE 7.5 file: the format stores the image in two files named '<base>.hdr' and '<base>.img'. Use a file name ending in '.hdr', '.img', '.hdr.gz' or '.img.gz', or a name without any extension to have the two file names derived from it.\n", filepath))
    }
  }

  pair <- analyze.pair.files(filepath)

  header_fh <- fileopen.write.gz.or.not(pair$header)
  write.analyze.header.internal(header_fh, analyzeheader)
  close(header_fh)

  data_fh <- fileopen.write.gz.or.not(pair$image)
  data_written <- analyze.write.data.internal(data_fh, analyzedata, analyzeheader)
  close(data_fh)

  return(invisible(list("header" = analyzeheader, "data" = data_written)))
}


#' @title Check whether an ANALYZE 7.5 header is suitable for writing.
#'
#' @param analyzeheader named list, an ANALYZE 7.5 header as returned by \code{\link{analyzeheader.template}}.
#'
#' @return logical, whether the header passed the checks. Fields that cannot be written as requested are reported
#'   with a `message`.
#'
#' @note The checks are in no way meant to be exhaustive. They only verify that the fields that are written to the
#'   file have a length that fits into their fixed size slots in the 348 byte header, since writing a longer value
#'   would shift all fields behind it and produce a corrupt file.
#'
#' @keywords internal
analyze.header.check <- function(analyzeheader) {
  is_okay <- TRUE

  if (!is.numeric(analyzeheader$sizeof_hdr) || analyzeheader$sizeof_hdr != 348L) {
    message(sprintf("Invalid 'sizeof_hdr' field: expected 348, found '%s'.\n", paste(analyzeheader$sizeof_hdr, collapse = " ")))
    is_okay <- FALSE
  }

  char_fields <- c("data_type" = 10L, "db_name" = 18L, "regular" = 1L, "hkey_un0" = 1L, "vox_units" = 4L, "cal_units" = 8L, "descrip" = 80L, "aux_file" = 24L, "originator" = 10L, "generated" = 10L, "scannum" = 10L, "patient_id" = 10L, "exp_date" = 10L, "exp_time" = 10L, "hist_un0" = 3L)
  for (field_name in names(char_fields)) {
    field_value <- analyzeheader[[field_name]]
    if (is.null(field_value)) {
      message(sprintf("Missing '%s' field in ANALYZE 7.5 header.\n", field_name))
      is_okay <- FALSE
    } else if (nchar(field_value) > char_fields[[field_name]]) {
      message(sprintf("Invalid '%s' field length: the ANALYZE 7.5 header has %d bytes for it, but the value has %d characters.\n", field_name, char_fields[[field_name]], nchar(field_value)))
      is_okay <- FALSE
    }
  }

  num_fields <- c("dim" = 8L, "pix_dim" = 8L)
  for (field_name in names(num_fields)) {
    if (length(analyzeheader[[field_name]]) != num_fields[[field_name]]) {
      message(sprintf("Invalid '%s' field length: expected %d, found %d.\n", field_name, num_fields[[field_name]], length(analyzeheader[[field_name]])))
      is_okay <- FALSE
    }
  }

  if (is.null(analyzeheader$datatype) || is.null(analyzeheader$bitpix)) {
    message("Missing 'datatype' or 'bitpix' field in ANALYZE 7.5 header.\n")
    is_okay <- FALSE
  }

  return(is_okay)
}


#' @title Open a connection for writing, with gzip support based on the file name.
#'
#' @param filepath character string, the path of the file to write.
#'
#' @return a connection, either a `gzfile` or a plain file connection.
#'
#' @keywords internal
fileopen.write.gz.or.not <- function(filepath) {
  if (guess.filename.is.gzipped(filepath, gz_extensions = c(".gz"))) {
    fh <- gzfile(filepath, "wb")
  } else {
    fh <- file(filepath, "wb", blocking = TRUE)
  }
  return(fh)
}


#' @title Write the 348 byte ANALYZE 7.5 header to a connection.
#'
#' @param fh connection to write to.
#'
#' @param analyzeheader named list, a valid ANALYZE 7.5 header.
#'
#' @return invisible `NULL`, called for the side effect of writing to the connection.
#'
#' @keywords internal
write.analyze.header.internal <- function(fh, analyzeheader) {
  endian <- analyzeheader$endian

  writeBin(as.integer(analyzeheader$sizeof_hdr), fh, size = 4L, endian = endian)

  write.analyze.char.field(fh, analyzeheader$data_type, 10L)
  write.analyze.char.field(fh, analyzeheader$db_name, 18L)
  writeBin(as.integer(analyzeheader$extents), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$session_error), fh, size = 2L, endian = endian)
  write.analyze.char.field(fh, analyzeheader$regular, 1L)
  write.analyze.char.field(fh, analyzeheader$hkey_un0, 1L)

  writeBin(as.integer(analyzeheader$dim), fh, size = 2L, endian = endian)

  write.analyze.char.field(fh, analyzeheader$vox_units, 4L)
  write.analyze.char.field(fh, analyzeheader$cal_units, 8L)
  writeBin(as.integer(analyzeheader$unused1), fh, size = 2L, endian = endian)
  writeBin(as.integer(analyzeheader$datatype), fh, size = 2L, endian = endian)
  writeBin(as.integer(analyzeheader$bitpix), fh, size = 2L, endian = endian)
  writeBin(as.integer(analyzeheader$dim_un0), fh, size = 2L, endian = endian)

  writeBin(as.double(analyzeheader$pix_dim), fh, size = 4L, endian = endian)
  writeBin(as.double(analyzeheader$vox_offset), fh, size = 4L, endian = endian)

  writeBin(as.double(analyzeheader$funused1), fh, size = 4L, endian = endian)
  writeBin(as.double(analyzeheader$funused2), fh, size = 4L, endian = endian)
  writeBin(as.double(analyzeheader$funused3), fh, size = 4L, endian = endian)

  writeBin(as.double(analyzeheader$cal_max), fh, size = 4L, endian = endian)
  writeBin(as.double(analyzeheader$cal_min), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$compressed), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$verified), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$glmax), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$glmin), fh, size = 4L, endian = endian)

  write.analyze.char.field(fh, analyzeheader$descrip, 80L)
  write.analyze.char.field(fh, analyzeheader$aux_file, 24L)

  # The 'orient' field is a single byte in the format definition, but it holds a small integer (the orientation
  # code). Out of range values are written as given, it is up to the reader to make sense of them.
  writeBin(as.integer(analyzeheader$orient), fh, size = 1L)

  # The 'originator' field is a char[10] in the format definition, but SPM stores the image origin in its first 6
  # bytes. If the header contains an 'spm_origin', it takes precedence, since it is the more specific value.
  originator_bytes <- write.analyze.char.field(fh, analyzeheader$originator, 10L, return_bytes = TRUE)
  if (!is.null(analyzeheader$spm_origin)) {
    if (nchar(analyzeheader$originator) > 0L) {
      warning(sprintf("The ANALYZE header contains both an 'originator' string ('%s') and an 'spm_origin' (%s). The SPM origin is written, since it is stored in the same bytes and SPM cannot read the string.\n", analyzeheader$originator, paste(analyzeheader$spm_origin, collapse = " ")))
    }
    if (length(analyzeheader$spm_origin) != 3L) {
      stop(sprintf("Invalid 'spm_origin' field: expected an integer vector of length 3, found length %d.\n", length(analyzeheader$spm_origin)))
    }
    originator_bytes <- writeBin(as.integer(analyzeheader$spm_origin), raw(), size = 2L, endian = endian)
  }
  writeBin(originator_bytes, fh)

  write.analyze.char.field(fh, analyzeheader$generated, 10L)
  write.analyze.char.field(fh, analyzeheader$scannum, 10L)
  write.analyze.char.field(fh, analyzeheader$patient_id, 10L)
  write.analyze.char.field(fh, analyzeheader$exp_date, 10L)
  write.analyze.char.field(fh, analyzeheader$exp_time, 10L)
  write.analyze.char.field(fh, analyzeheader$hist_un0, 3L)

  writeBin(as.integer(analyzeheader$views), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$vols_added), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$start_field), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$field_skip), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$omax), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$omin), fh, size = 4L, endian = endian)
  writeBin(as.integer(analyzeheader$smax), fh, size = 4L, endian = endian)

  # The last four bytes are 'smin' in ANALYZE and the magic field in NIFTI. For ANALYZE files, zeroes are written
  # here, which is what makes the file recognizable as an ANALYZE file (see is.analyze.file). A 'magic' string in
  # the header is written instead, which produces a NIFTI v1 pair file that describes a different header layout.
  if (is.null(analyzeheader$magic) || identical(analyzeheader$magic, "")) {
    writeBin(as.integer(analyzeheader$smin), fh, size = 4L, endian = endian)
  } else {
    write.analyze.char.field(fh, analyzeheader$magic, 4L)
  }

  return(invisible(NULL))
}


#' @title Write a fixed length character field of an ANALYZE 7.5 header.
#'
#' @param fh connection to write to.
#'
#' @param field_value character string, the value to write.
#'
#' @param field_length integer, the length of the field in bytes.
#'
#' @param return_bytes logical, whether to return the raw bytes instead of writing them to the connection.
#'
#' @return invisible `NULL` (or the raw vector of length `field_length` if `return_bytes` is `TRUE`), called for
#'   the side effect of writing to the connection.
#'
#' @note The string is converted to bytes as ISO-8859-1 (latin-1), which is the inverse of the conversion that
#'   \code{\link{analyze.read.char.field}} applies when reading, so that the content of the field survives a read
#'   and write cycle byte for byte. Characters that latin-1 cannot represent are written as UTF-8 instead.
#'
#' @keywords internal
write.analyze.char.field <- function(fh, field_value, field_length, return_bytes = FALSE) {
  if (is.null(field_value)) {
    field_value <- ""
  }
  latin1_value <- iconv(field_value, from = "UTF-8", to = "ISO-8859-1")
  if (is.na(latin1_value)) {
    latin1_value <- field_value
  }
  value_bytes <- charToRaw(latin1_value)
  if (length(value_bytes) > field_length) {
    stop(sprintf("Character field of length %d is too short for the value '%s' (%d bytes).\n", field_length, field_value, length(value_bytes)))
  }
  field_bytes <- c(value_bytes, rep(as.raw(0), field_length - length(value_bytes)))
  if (return_bytes) {
    return(field_bytes)
  }
  writeBin(field_bytes, fh)
  return(invisible(NULL))
}


#' @title Write the voxel data of an ANALYZE 7.5 image to a connection.
#'
#' @param fh connection to write to.
#'
#' @param analyzedata array of numeric data.
#'
#' @param analyzeheader named list, a valid ANALYZE 7.5 header that describes the data.
#'
#' @return the data that was written, after conversion to the data type stated in the header.
#'
#' @keywords internal
analyze.write.data.internal <- function(fh, analyzedata, analyzeheader) {
  endian <- analyzeheader$endian
  num_bytes <- as.integer(analyzeheader$bitpix / 8L)

  if (as.integer(analyzeheader$datatype) %in% c(2L, 4L, 8L, 256L, 512L, 768L)) { # integer data types
    if (!is.integer(analyzedata)) {
      warning(sprintf("Found ANALYZE integer data type '%d' in the header, but the data is not of type integer. Converting the data to integer as specified in the header.\n", analyzeheader$datatype))
    }
    data_written <- as.integer(analyzedata)
  } else { # floating point data types
    if (!is.double(analyzedata)) {
      warning(sprintf("Found ANALYZE floating point data type '%d' in the header, but the data is not of type double. Converting the data to double as specified in the header.\n", analyzeheader$datatype))
    }
    data_written <- as.double(analyzedata)
  }

  writeBin(data_written, fh, size = num_bytes, endian = endian)
  return(data_written)
}
