#' @title Create a template NIFTI v1 header. You will have to adapt it for your use case.
#'
#' @param pair logical, whether the template describes a NIFTI v1 *pair* file (a `.hdr` header file plus a separate
#'   `.img` data file, the variant that is also used by the ANALYZE 7.5 format and that FSL writes) instead of a
#'   single file (`.nii`, the header and the data in one file). The two variants differ in the `magic` field and in
#'   the `vox_offset` field, which is 352 for a single file (the data starts behind the header) and 0 for a pair (the
#'   data starts at the first byte of the `.img` file). Both variants are written by \code{\link{write.nifti1}}.
#'
#' @return named list, the NIFTI v1 header. All fields are present and filled with values of a proper type. Whether or not they make sense is up to you, but you will most likely have to adapt at least the following fields to your data: `dim_raw`, `datatype`, `bitpix`.
#'
#' @note Commonly used data type settings are: for signed integers datatype = `8L` and bitpix = `32L`; for floats datatype = `16L` and bitpix = `32L`. See the NIFTI v1 standard for more options.  You may want to call \code{\link{ni1header.for.data}} instead of this function.
#'
#' @seealso \code{\link{ni1header.for.data}}
#'
#' @export
ni1header.template <- function(pair = FALSE) {
  niiheader <- list("endian" = "little")

  niiheader$sizeof_hdr <- 348L

  niiheader$dim <- c(3, 256, 256, 256, 1, 1, 1, 1)

  niiheader$intent_p1 <- 0.0
  niiheader$intent_p2 <- 0.0
  niiheader$intent_p3 <- 0.0

  niiheader$intent_code <- 0L
  niiheader$datatype <- 16L
  niiheader$bitpix <- 4 * 8L
  niiheader$slice_start <- 0L

  niiheader$pix_dim <- rep(0.0, 8L)
  niiheader$vox_offset <- ifelse(pair, 0., 352.)
  niiheader$scl_slope <- 0.0
  niiheader$scl_inter <- 0.0

  niiheader$slice_end <- 0L
  niiheader$slice_code <- 0L
  niiheader$xyzt_units <- 0L

  niiheader$cal_max <- 255.0
  niiheader$cal_min <- 0.0
  niiheader$slice_duration <- 0.0
  niiheader$toffset <- 0.0

  niiheader$glmax <- 0L
  niiheader$glmin <- 0L

  niiheader$descrip <- "nifti1file" # max 80 bytes
  niiheader$aux_file <- "" # max 24 bytes

  niiheader$qform_code <- 0L
  niiheader$sform_code <- 0L

  niiheader$quatern_b <- 0.0
  niiheader$quatern_c <- 0.0
  niiheader$quatern_d <- 0.0

  niiheader$qoffset_x <- 0.0
  niiheader$qoffset_y <- 0.0
  niiheader$qoffset_z <- 0.0

  niiheader$srow_x <- rep(0.0, 4L)
  niiheader$srow_y <- rep(0.0, 4L)
  niiheader$srow_z <- rep(0.0, 4L)

  niiheader$intent_name <- "" # max 16 bytes
  niiheader$magic <- ifelse(pair, "ni1", "n+1") # max 4 bytes

  return(niiheader)
}


#' @title Create NIFTI v1 header suitable for given data.
#'
#' @param niidata array of numeric (integer or double) data, can have up to 7 dimensions.
#'
#' @param allow_fshack logical, whether to allow data in which the first dimension is larger than 32767, and use the FreeSurfer NIFTI v1 hack to support his. The hack will be used only if needed. WARNING: Files written with the hack do not conform to the NIFTI v1 standard and will not be read correctly by most software. All FreeSurfer tools and the Python 'nibabel' module support it.
#'
#' @param pair logical, whether the header should describe a NIFTI v1 pair (a `.hdr` header file plus a separate
#'   `.img` data file) instead of a single file. See \code{\link{ni1header.template}}.
#'
#' @return a NIFTI v1 header (see \code{\link{ni1header.template}}) in which the datatype, bitpix, dim and dim_raw fields have been set to values suitable for the given data. Feel free to change the other fields.
ni1header.for.data <- function(niidata, allow_fshack = FALSE, pair = FALSE) {
  niiheader <- ni1header.template(pair = pair)

  if (is.integer(niidata)) {
    niiheader$datatype <- 8L
    niiheader$bitpix <- 32L
  } else if (is.double(niidata)) {
    niiheader$datatype <- 16L
    niiheader$bitpix <- 32L
  } else {
    stop("Only integer or double data is supported by this function.")
  }

  if (is.vector(niidata)) {
    dd <- length(niidata)
  } else {
    dd <- dim(niidata)
  }
  niiheader$dim <- nifti.datadim.to.dimfield(dd)
  niiheader$cal_min <- min(niidata)
  niiheader$cal_max <- max(niidata)

  nii1_max_vox <- 32767L

  if (dd[1] > nii1_max_vox) {
    if (allow_fshack) {
      niiheader$glmin <- dd[1]
      niiheader$dim[2] <- -1L
      niiheader$magic <- ""
    } else {
      stop(sprintf("Data dimension #1: %d too large for NIFTI v1 without FreeSurfer hack, limit is %d.\n", dd[1], nii1_max_vox))
    }
  }

  if (any(niiheader$dim > nii1_max_vox)) {
    stop("Data dimensions too large for NIFTI v1 format, consider using NIFTI v2.")
  }

  nifti.header.check(niiheader, nifti_version = 1L)
  return(niiheader)
}


#' @title Write header and data to a file in NIFTI v1 format.
#'
#' @param filepath the file to write. The extension should be '.nii' or '.nii.gz' for a single file NIFTI image, and
#'   '.hdr', '.img' (optionally followed by '.gz') for a NIFTI v1 pair, i.e. a header file with the voxel data in a
#'   separate '.img' file. Which of the two is written follows from the `magic` field of the header: 'n+1' means a
#'   single file, 'ni1' means a pair. See \code{\link{ni1header.template}}.
#'
#' @param niidata array of numeric or integer data, with up to 7 dimensions. Will be written to the file with the datatype and bitpix specified in the 'niiheader' argument. Set to `NULL` and pass a 'niiheader' to write only the header, and remember to adapt 'magic' in the header.
#'
#' @param niiheader an optional NIFTI v1 header that is suitable for the passed 'niidata'. If not given, one will be generated with \code{\link{ni1header.for.data}}.
#'
#' @param ... additional parameters passed to \code{\link{ni1header.for.data}}. Only used if 'niiheader' is `NULL`.
#'
#' @return invisible named list with the entries `header` (the header that was written) and `data` (the data that
#'   was written, or `NULL` if no data was passed).
#'
#' @family nifti1 writers
#'
#' @examples
#' # A single file NIFTI image (the default):
#' data <- array(1:24, dim = c(4, 3, 2))
#' nii_file <- tempfile(fileext = ".nii")
#' write.nifti1(nii_file, data)
#'
#' # A NIFTI v1 pair, i.e. a header plus a separate data file. Ask for a pair
#' # header by passing 'pair = TRUE' (this is passed on to the header generator):
#' write.nifti1(tempfile(fileext = ".hdr"), data, pair = TRUE)
#'
#' # The NIFTI v1 pair files can be read back with the volume reader:
#' hdr_file <- tempfile(fileext = ".hdr")
#' write.nifti1(hdr_file, data, pair = TRUE)
#' vol <- read.fs.volume(hdr_file, with_header = TRUE)
#' print(dim(vol$data))
#'
#' @export
write.nifti1 <- function(filepath, niidata, niiheader = NULL, ...) {
  if (is.null(niiheader)) {
    niiheader <- ni1header.for.data(niidata, ...)
  }

  if (!nifti.header.check(niiheader, nifti_version = 1L)) {
    stop("Invalid NIFTI v1 header.")
  }

  pair_files <- c(".hdr", ".img", ".hdr.gz", ".img.gz")
  data_written <- NULL

  if (!is.null(niiheader$magic) && identical(niiheader$magic, "ni1")) {
    # A NIFTI v1 pair: two files, and the data starts at the first byte of the '.img' file.
    if (!filepath.ends.with(filepath, pair_files) && grepl(".", basename(filepath), fixed = TRUE)) {
      stop(sprintf("Cannot write a NIFTI v1 pair (magic 'ni1') to '%s': a pair consists of the two files '<base>.hdr' and '<base>.img'. Use a file name ending in '.hdr', '.img', '.hdr.gz' or '.img.gz', or a name without any extension to have the two file names derived from it. Set the header magic to 'n+1' to write a single file NIFTI image.\n", filepath))
    }
    if (niiheader$vox_offset != 0.) {
      warning(sprintf("Setting the 'vox_offset' field of the NIFTI v1 pair header to 0 (it was %.1f): for pair files the voxel data starts at the first byte of the '.img' file, so the field is meaningless.\n", niiheader$vox_offset))
      niiheader$vox_offset <- 0.
    }

    pair <- analyze.pair.files(filepath, require_header = FALSE)

    header_fh <- fileopen.write.gz.or.not(pair$header)
    write.nifti1.header.internal(header_fh, niiheader)
    close(header_fh)

    data_fh <- fileopen.write.gz.or.not(pair$image)
    data_written <- write.nifti1.data.internal(data_fh, niidata, niiheader)
    close(data_fh)
  } else {
    # A single file NIFTI image: header and data in one file.
    if (filepath.ends.with(filepath, pair_files)) {
      stop(sprintf("Cannot write a single file NIFTI image (magic '%s') to '%s': the file name describes the header or the data file of a pair. Set the header magic to 'ni1' to write a pair, see ni1header.template(pair = TRUE).\n", niiheader$magic, filepath))
    }

    fh <- fileopen.write.gz.or.not(filepath)
    write.nifti1.header.internal(fh, niiheader)
    data_written <- write.nifti1.data.internal(fh = fh, niidata = niidata, niiheader = niiheader)
    close(fh)
  }

  return(invisible(list("header" = niiheader, "data" = data_written)))
}


#' @title Write the 348 byte NIFTI v1 header (and the padding up to the data offset) to a connection.
#'
#' @param fh connection to write to.
#'
#' @param niiheader named list, a valid NIFTI v1 header.
#'
#' @return invisible `NULL`, called for the side effect of writing to the connection.
#'
#' @keywords internal
write.nifti1.header.internal <- function(fh, niiheader) {
  endian <- niiheader$endian

  writeBin(as.integer(niiheader$sizeof_hdr), fh, size = 4L, endian = endian)
  writeBin(as.integer(rep(0L, 36L)), fh, size = 1L, endian = endian) # Unused header part, for compatibility with old formats like ANALYZE. We fill it with zeroes.
  writeBin(as.integer(niiheader$dim), fh, size = 2L, endian = endian)

  writeBin(as.double(niiheader$intent_p1), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$intent_p2), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$intent_p3), fh, size = 4L, endian = endian)

  writeBin(as.integer(niiheader$intent_code), fh, size = 2L, endian = endian)
  writeBin(as.integer(niiheader$datatype), fh, size = 2L, endian = endian)
  writeBin(as.integer(niiheader$bitpix), fh, size = 2L, endian = endian)
  writeBin(as.integer(niiheader$slice_start), fh, size = 2L, endian = endian)

  writeBin(as.double(niiheader$pix_dim), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$vox_offset), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$scl_slope), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$scl_inter), fh, size = 4L, endian = endian)

  writeBin(as.integer(niiheader$slice_end), fh, size = 2L, endian = endian)
  writeBin(as.integer(niiheader$slice_code), fh, size = 1L, endian = endian)
  writeBin(as.integer(niiheader$xyzt_units), fh, size = 1L, endian = endian)

  writeBin(as.double(niiheader$cal_max), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$cal_min), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$slice_duration), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$toffset), fh, size = 4L, endian = endian)

  writeBin(as.integer(niiheader$glmax), fh, size = 4L, endian = endian)
  writeBin(as.integer(niiheader$glmin), fh, size = 4L, endian = endian)

  if (nchar(niiheader$descrip) > 0L) {
    writeChar(niiheader$descrip, fh, eos = NULL)
  }
  writeBin(as.raw(rep(0L, (80L - nchar(niiheader$descrip)))), fh, endian = endian) # fill remaining space up to max 80 bytes with zeroes.

  if (nchar(niiheader$aux_file) > 0L) {
    writeChar(niiheader$aux_file, fh, eos = NULL)
  }
  writeBin(as.raw(rep(0L, (24L - nchar(niiheader$aux_file)))), fh, endian = endian) # fill remaining space up to max 24 bytes with zeroes.

  writeBin(as.integer(niiheader$qform_code), fh, size = 2L, endian = endian)
  writeBin(as.integer(niiheader$sform_code), fh, size = 2L, endian = endian)

  writeBin(as.double(niiheader$quatern_b), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$quatern_c), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$quatern_d), fh, size = 4L, endian = endian)

  writeBin(as.double(niiheader$qoffset_x), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$qoffset_y), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$qoffset_z), fh, size = 4L, endian = endian)

  writeBin(as.double(niiheader$srow_x), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$srow_y), fh, size = 4L, endian = endian)
  writeBin(as.double(niiheader$srow_z), fh, size = 4L, endian = endian)

  if (nchar(niiheader$intent_name) > 0L) {
    writeChar(niiheader$intent_name, fh, eos = NULL)
  }
  writeBin(as.raw(rep(0L, (16L - nchar(niiheader$intent_name)))), fh, endian = endian) # fill remaining space up to max 16 bytes with zeroes.

  if (nchar(niiheader$magic) > 0L) {
    writeChar(niiheader$magic, fh, eos = NULL)
  }
  writeBin(as.raw(rep(0L, (4L - nchar(niiheader$magic)))), fh, endian = endian) # fill remaining space up to max 4 bytes with zeroes.

  # add zero padding up to 'vox_offset'. For pair files the data is stored in a separate file and 'vox_offset' is 0,
  # so there is nothing to pad.
  position_now <- 348L
  num_to_fill <- as.integer(niiheader$vox_offset) - position_now
  if (num_to_fill > 0L) {
    writeBin(as.raw(rep(0L, num_to_fill)), fh, endian = endian) # fill remaining space with zeroes.
  } else if (num_to_fill < 0L && niiheader$vox_offset != 0.) {
    stop(sprintf("Invalid 'vox_offset' field %.1f in the NIFTI v1 header: it must be 0 (data in a separate file, see 'magic') or at least 348, the size of the header.\n", niiheader$vox_offset))
  }

  return(invisible(NULL))
}


#' @title Write the voxel data of a NIFTI v1 file to a connection.
#'
#' @param fh connection to write to.
#'
#' @param niidata array of numeric data, or `NULL` to write no data at all.
#'
#' @param niiheader named list, a valid NIFTI v1 header that describes the data.
#'
#' @return the data that was written, after conversion to the data type stated in the header, or `NULL` if `niidata`
#'   was `NULL`.
#'
#' @keywords internal
write.nifti1.data.internal <- function(fh, niidata, niiheader) {
  endian <- niiheader$endian
  data_written <- NULL

  if (!is.null(niidata)) {
    # Write data.
    if (as.integer(niiheader$datatype) %in% c(2L, 4L, 8L, 512L, 768L)) { # integer NIFTI data types
      if (!is.integer(niidata)) {
        warning("Found NIFTI integer datatype '%d' in niiheader, but niidata datatype is not integer. Converting data to integer as specified in header.\n", niiheader$datatype)
      }
      data_written <- as.integer(niidata)
      writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian)
    } else { # treat as double
      if (!is.double(niidata)) {
        warning("Found NIFTI floating point datatype '%d' in niiheader, but niidata datatype is not floating point. Converting data to float as specified in header.\n", niiheader$datatype)
      }
      data_written <- as.double(niidata)
      writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian)
    }
  }

  return(data_written)
}
