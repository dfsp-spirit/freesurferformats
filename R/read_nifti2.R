#' @title Read NIFTI v2 header from file.
#'
#' @param filepath path to a NIFTI v2 file.
#'
#' @return named list with NIFTI 2 header fields. The header extensions, if the file has any, are returned in
#'   the field `extensions`, a list in which each entry is one header extension as created by
#'   \code{\link{nifti2.extension}} (i.e., a list with the entries 'ecode' and 'content'). CIFTI2 files store
#'   their XML metadata in such an extension, see \code{\link{nifti2.get.extension}}.
#'
#' @seealso \code{\link{write.nifti2}}
#'
#' @export
read.nifti2.header <- function(filepath) {
  niiheader <- read.nifti2.header.internal(filepath, little_endian = TRUE)
  return(niiheader)
}


#' @title Read NIFTI v2 header from file.
#'
#' @inheritParams read.nifti2.header
#'
#' @param little_endian internal logical, leave this alone. Endianness will be figured out automatically, messing with this parameter only hurts.
#'
#' @return named list with NIFTI 2 header fields, including the header extensions (field `extensions`, see \code{\link{read.nifti2.header}}).
#'
#' @note See https://nifti.nimh.nih.gov/pub/dist/data/nifti2/ for test data. Thanks to Anderson Winkler for his post at https://brainder.org/2015/04/03/the-nifti-2-file-format/.
#'
#' @keywords internal
read.nifti2.header.internal <- function(filepath, little_endian = TRUE) {
  endian <- "little"
  if (!little_endian) {
    endian <- "big"
  }
  niiheader <- list("endian" = endian)

  fh <- fileopen.gz.or.not(filepath)
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )

  niiheader$sizeof_hdr <- readBin(fh, integer(), n = 1, size = 4, endian = endian)

  if (niiheader$sizeof_hdr != 540L) {
    if (niiheader$sizeof_hdr == 348L) {
      stop("File not in NIFTI 2 format: header size 348 looks like a NIFTI v1 file.")
    }
    if (little_endian == FALSE) { # if called with FALSE, the TRUE option was already checked.
      stop(sprintf("File not in NIFTI 2 format: invalid header size %d, expected 540.\n", niiheader$sizeof_hdr)) # nocov
    } else {
      return(read.nifti2.header.internal(filepath, little_endian = FALSE))
    }
  }

  niiheader$magic <- read.fixed.char.binary(fh, 8L)

  # The magic of a NIFTI v2 file is the string 'n+2'. Versions of this package before 1.1.0 wrote 'n+1' here,
  # which is the NIFTI v1 magic: the files are still readable here, but other software rejects them
  # (nibabel reports 'magic string n+1 is not valid', Connectome Workbench reports 'incorrect magic').
  if (!is.na(niiheader$magic) && nchar(niiheader$magic) >= 3L && identical(substr(niiheader$magic, 1L, 3L), "n+1")) {
    warning("The magic string of this NIFTI v2 file is 'n+1', which is the NIFTI v1 magic. This violates the NIFTI v2 standard, and other software (nibabel, Connectome Workbench) refuses to read such a file. Files written by versions of this package before 1.1.0 have this problem. The file can be fixed by writing it again with a NIFTI v2 header created by this package, see ?write.nifti2.\n")
  }
  niiheader$datatype <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  niiheader$bitpix <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  niiheader$dim <- readBin(fh, integer(), n = 8, size = 8, endian = endian)

  niiheader$intent_p1 <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$intent_p2 <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$intent_p3 <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)

  niiheader$pix_dim <- readBin(fh, numeric(), n = 8, size = 8, endian = endian)

  niiheader$vox_offset <- readBin(fh, integer(), n = 1, size = 8, endian = endian)

  niiheader$scl_slope <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$scl_inter <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)

  niiheader$cal_max <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$cal_min <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$slice_duration <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$toffset <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)

  niiheader$slice_start <- readBin(fh, integer(), n = 1, size = 8, endian = endian)
  niiheader$slice_end <- readBin(fh, integer(), n = 1, size = 8, endian = endian)

  # niiheader$descrip = readBin(fh, character(), n = 1, endian = endian); # 80 bytes
  # niiheader$aux_file = readBin(fh, character(), n = 1, endian = endian); # 24 bytes
  niiheader$descrip <- read.fixed.char.binary(fh, 80L)
  niiheader$aux_file <- read.fixed.char.binary(fh, 24L)

  niiheader$qform_code <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  niiheader$sform_code <- readBin(fh, integer(), n = 1, size = 4, endian = endian)

  niiheader$quatern_b <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$quatern_c <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$quatern_d <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)

  niiheader$qoffset_x <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$qoffset_y <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)
  niiheader$qoffset_z <- readBin(fh, numeric(), n = 1, size = 8, endian = endian)

  niiheader$srow_x <- readBin(fh, numeric(), n = 4, size = 8, endian = endian)
  niiheader$srow_y <- readBin(fh, numeric(), n = 4, size = 8, endian = endian)
  niiheader$srow_z <- readBin(fh, numeric(), n = 4, size = 8, endian = endian)

  niiheader$slice_code <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  niiheader$xyzt_units <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  niiheader$intent_code <- readBin(fh, integer(), n = 1, size = 4, endian = endian)

  # niiheader$intent_name = readBin(fh, character(), n = 1, endian = endian); # 16 bytes
  niiheader$intent_name <- read.fixed.char.binary(fh, 16L)
  niiheader$dim_info <- readBin(fh, integer(), n = 1, size = 1, endian = endian)

  # 'unused_str': 15 reserved bytes, they are always zero in practice.
  unused_str <- readBin(fh, integer(), n = 15L, size = 1L, endian = endian)
  unused_str <- NULL

  # The 4 bytes after the fixed-size header tell whether header extensions follow: if the first byte
  # is non-zero, one or more extensions follow, and the file offset of the data ('vox_offset') is
  # larger than 544. CIFTI2 files store their XML metadata in such an extension, see
  # R/nifti2_extensions.R.
  extender <- readBin(fh, integer(), n = 4L, size = 1L, endian = endian)
  niiheader$extensions <- list()
  if (length(extender) == 4L && extender[1L] != 0L) {
    niiheader$extensions <- nifti2.read.extensions(fh, as.integer(niiheader$vox_offset) - 544L, endian)
  }

  return(niiheader)
}


#' @title Read raw data from NIFTI v2 file.
#'
#' @inheritParams read.nifti2.header
#'
#' @param header optional nifti v2 header obtained from \code{\link{read.nifti2.header}}. Will be loaded automatically if left at `NULL`.
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions in the loaded data array.
#'
#' @return the data in the NIFTI v2 file. Note that the NIFTI v2 header information (scaling, units, etc.) is not applied in any way: the data are returned raw, as read from the file. The information in the header is used to read the data with the proper data type and size.
#'
#' @export
read.nifti2.data <- function(filepath, header = NULL, drop_empty_dims = TRUE) {
  if (is.null(header)) {
    header <- read.nifti2.header(filepath)
  }

  fh <- fileopen.gz.or.not(filepath)
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )

  endian <- header$endian

  # move to data part
  num_skip <- header$vox_offset
  discarded <- readBin(fh, integer(), n = num_skip, size = 1L, endian = endian)
  discarded <- NULL

  data_dim <- nifti.datadim.from.dimfield(header$dim)
  num_values <- prod(data_dim)

  read_size_bytes <- header$bitpix / 8L # bitpix is the size in bits, but we need bytes.

  # Security: validate allocation size before reading data
  validate_allocation_size(data_dim, read_size_bytes)

  data <- read.nifti.values(fh, header$datatype, header$bitpix, num_values, endian)
  data <- array(data, dim = data_dim)
  if (drop_empty_dims) {
    return(drop(data))
  }
  return(data)
}
