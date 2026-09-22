# Read and write support for the ANALYZE 7.5 image format (and for the two-file variant of NIFTI v1).
#
# The ANALYZE 7.5 format stores a 3D (or 4D) image in two files: a 348 byte header ('<base>.hdr') and the raw voxel
# data ('<base>.img'). The header has the same size and, for the fields shared with NIFTI v1, the same offsets, but
# it is NOT the NIFTI v1 header: the fields that NIFTI invented (intent codes, slice timing, the units fields, the
# qform/sform geometry and the magic string) reuse space that ANALYZE assigns to other things (vox_units, cal_units,
# dim_un0, funused1-3, compressed, verified, orient, originator, generated, ..., smin). Reading an ANALYZE file with
# a NIFTI reader therefore yields plausible looking garbage for all of these, which is why this file has its own
# header reader and writer instead of a flag in read.nifti1.header().
#
# The last 4 bytes (348-4) are the 'smin' field in ANALYZE and the 'magic' field in NIFTI. They are what tells the
# two variants of a '.hdr'/'.img' pair apart: the string 'ni1' means that the pair holds a NIFTI v1 image (which is
# what FSL writes), an empty magic means plain ANALYZE 7.5. Both variants are supported here, see
# read.fs.volume.analyze().


#' @title Create a template ANALYZE 7.5 header. You will have to adapt it for your use case.
#'
#' @description This function returns a valid ANALYZE 7.5 header with all fields present, filled with the default
#'   values of the format. You will most likely have to adapt at least the `dim`, `datatype`, `bitpix` and `pix_dim`
#'   fields to your data, which is what \code{\link{analyzeheader.for.data}} does for you.
#'
#' @return named list, the ANALYZE 7.5 header. The fields `endian`, `magic`, `header_format` and `spm_origin` are not
#'   part of the ANALYZE 7.5 header: `endian` and `magic` describe how the file is stored, `header_format` tells
#'   which variant of the 348 byte header this is ('analyze' or 'nifti1_pair'), and `spm_origin` is the
#'   interpretation of the `originator` field that the SPM software uses.
#'
#' @note The fields `spm_origin` and `magic` are derived when a file is read, they are documented in
#'   \code{\link{read.analyze.header}}.
#'
#' @seealso \code{\link{analyzeheader.for.data}}, \code{\link{read.analyze.header}}, \code{\link{write.analyze}}
#'
#' @examples
#' analyzeheader <- analyzeheader.template()
#' analyzeheader$dim <- c(3L, 4L, 3L, 2L, 1L, 1L, 1L, 1L)
#' analyzeheader$pix_dim <- c(1., 1., 1., 1., 0., 0., 0., 0.)
#'
#' @export
analyzeheader.template <- function() {
  analyzeheader <- list("endian" = "little")

  analyzeheader$sizeof_hdr <- 348L

  analyzeheader$data_type <- "" # char[10], unused in practice
  analyzeheader$db_name <- "" # char[18], unused in practice
  analyzeheader$extents <- 0L
  analyzeheader$session_error <- 0L
  analyzeheader$regular <- "" # char[1]
  analyzeheader$hkey_un0 <- "" # char[1]

  analyzeheader$dim <- c(3L, 256L, 256L, 256L, 1L, 1L, 1L, 1L)

  analyzeheader$vox_units <- "" # char[4], e.g. 'mm', not used by most tools
  analyzeheader$cal_units <- "" # char[8], not used by most tools
  analyzeheader$unused1 <- 0L
  analyzeheader$datatype <- 16L
  analyzeheader$bitpix <- 32L
  analyzeheader$dim_un0 <- 0L

  analyzeheader$pix_dim <- rep(0., 8L) # the voxel sizes are pix_dim[2:4], pix_dim[1] is the (unused) 'qfac' field of NIFTI
  analyzeheader$vox_offset <- 0. # always 0 for ANALYZE, the data is in a separate file

  analyzeheader$funused1 <- 0. # SPM uses this as the scale factor
  analyzeheader$funused2 <- 0.
  analyzeheader$funused3 <- 0.

  analyzeheader$cal_max <- 0.
  analyzeheader$cal_min <- 0.
  analyzeheader$compressed <- 0L
  analyzeheader$verified <- 0L
  analyzeheader$glmax <- 0L
  analyzeheader$glmin <- 0L

  analyzeheader$descrip <- "" # char[80]
  analyzeheader$aux_file <- "" # char[24]
  analyzeheader$orient <- 0L # char[1] in the format definition, holds the orientation code 0-5
  analyzeheader$originator <- "" # char[10], SPM uses this as the image origin
  analyzeheader$generated <- "" # char[10]
  analyzeheader$scannum <- "" # char[10]
  analyzeheader$patient_id <- "" # char[10]
  analyzeheader$exp_date <- "" # char[10]
  analyzeheader$exp_time <- "" # char[10]
  analyzeheader$hist_un0 <- "" # char[3]

  analyzeheader$views <- 0L
  analyzeheader$vols_added <- 0L
  analyzeheader$start_field <- 0L
  analyzeheader$field_skip <- 0L
  analyzeheader$omax <- 0L
  analyzeheader$omin <- 0L
  analyzeheader$smax <- 0L
  analyzeheader$smin <- 0L

  # Fields that are not part of the ANALYZE 7.5 format definition.
  analyzeheader$magic <- "" # the 4 bytes at offset 344 (the 'smin' field), seen as a char string: '' for ANALYZE, 'ni1' or 'n+1' for NIFTI
  analyzeheader$header_format <- "analyze" # 'analyze' or 'nifti1_pair'
  analyzeheader$spm_origin <- NULL # the 3 int16 values that SPM stores in the 'originator' field
  analyzeheader$originator_bytes <- NULL # the raw bytes of the 'originator' field, see read.analyze.header

  return(analyzeheader)
}


#' @title Create ANALYZE 7.5 header suitable for given data.
#'
#' @param analyzedata array of numeric (integer or double) data, can have up to 7 dimensions.
#'
#' @param ... extra parameters passed on to \code{\link{analyzeheader.template}}, ignored in this function.
#'
#' @return an ANALYZE 7.5 header (see \code{\link{analyzeheader.template}}) in which the `datatype`, `bitpix`,
#'   `dim`, `cal_min` and `cal_max` fields have been set to values suitable for the given data. Feel free to change
#'   the other fields, e.g. the voxel sizes in `pix_dim` or the `orient` code.
#'
#' @note ANALYZE 7.5 has much fewer data types than NIFTI: integers are stored as 32 bit signed integers and floating
#'   point data as 32 bit floats, which is what this function selects (the same choice that
#'   \code{\link{ni1header.for.data}} makes for NIFTI v1). To store 16 bit integers, which is the classic ANALYZE
#'   data type, set `datatype` to `4L` and `bitpix` to `16L` in the returned header after calling this function.
#'
#' @examples
#' analyzeheader <- analyzeheader.for.data(array(1:24, dim = c(4, 3, 2)))
#' analyzeheader$datatype
#'
#' @export
analyzeheader.for.data <- function(analyzedata, ...) {
  analyzeheader <- analyzeheader.template()

  if (is.integer(analyzedata)) {
    analyzeheader$datatype <- 8L
    analyzeheader$bitpix <- 32L
  } else if (is.double(analyzedata)) {
    analyzeheader$datatype <- 16L
    analyzeheader$bitpix <- 32L
  } else {
    stop("Only integer or double data is supported by this function.")
  }

  if (is.vector(analyzedata)) {
    dd <- length(analyzedata)
  } else {
    dd <- dim(analyzedata)
  }
  if (length(dd) > 7L) {
    stop(sprintf("ANALYZE data can have at most 7 dimensions, but the data has %d.\n", length(dd)))
  }
  analyzeheader$dim <- nifti.datadim.to.dimfield(dd)
  analyzeheader$cal_min <- min(analyzedata)
  analyzeheader$cal_max <- max(analyzedata)

  return(analyzeheader)
}


#' @title Check whether a file is an ANALYZE 7.5 file.
#'
#' @param filepath character string, the path to a `.hdr` file (or to a file whose name ends with the base name of
#'   one, see \code{\link{analyze.pair.files}}).
#'
#' @return logical, whether the 348 byte header is an ANALYZE 7.5 header. This is the case when the header contains
#'   neither of the two NIFTI v1 magic strings, i.e. when the magic is empty. Files that use the `ni1` magic are
#'   NIFTI v1 pair files, see \code{\link{read.nifti1.header}}, and files with the `n+1` magic are single file NIFTI
#'   v1 files.
#'
#' @note Note that this function returns `TRUE` for any two-file header that does not carry a NIFTI magic, which is
#'   what "ANALYZE 7.5" means in practice: these files were written by ANALYZE itself, by SPM, by AFNI or by
#'   FreeSurfer. Only the fields of the ANALYZE specification are defined for them, but SPM stores extra
#'   information in fields that ANALYZE leaves unused, see \code{\link{read.fs.volume.analyze}}.
#'
#' @note A file path that does not describe an existing pair file (for example a single file NIFTI image, or a
#'   file that does not exist) returns `FALSE` rather than an error.
#'
#' @examples
#' hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' is.analyze.file(hdrfile)
#'
#' @export
is.analyze.file <- function(filepath) {
  pair <- analyze.pair.files(filepath)
  if (!pair$header_exists) {
    return(FALSE)
  }
  magic <- analyze.read.magic(pair$header)
  return(!(magic %in% c("ni1", "n+1")))
}


#' @title Determine the header and data file of an ANALYZE 7.5 or NIFTI v1 pair file.
#'
#' @description The two formats ANALYZE 7.5 and NIFTI v1 (in its two-file variant) store the image in a header file
#'   (`<base>.hdr`) and a data file (`<base>.img`). This function computes the two file names from any of them, so
#'   that the user can pass any of the possible spellings.
#'
#' @param filepath character string, the path to the header file, to the data file, or to the base name (with or
#'   without a `.hdr`/`.img`/`.gz` suffix).
#'
#' @param require_header logical, whether to stop with an error if the header file does not exist. If `FALSE`, the
#'   computed file names are returned even if the files are missing.
#'
#' @return named list with the entries `header` and `image`, the full paths to the header and the data file, the
#'   logical entries `header_exists` and `image_exists`, and the entries `mat` (the path of the MATLAB sidecar file
#'   that SPM and FreeSurfer write next to the image file, see \code{\link{read.fs.volume.analyze}}) and
#'   `mat_exists`, which report whether such a file is present.
#'
#' @note Compression is handled the way the other implementations of the format handle it: the suffix `.gz` (and
#'   `.hdr`/`.img`) is stripped from the file name to get the base name, and the two file names are constructed from
#'   it. A name like `vol.hdr.gz` therefore describes the pair `vol.hdr.gz` and `vol.img.gz`, and `vol.nii.gz` would
#'   be the pair `vol.nii.hdr.gz` and `vol.nii.img.gz`. Note that many tools only read uncompressed pairs, so
#'   compressed pairs should only be written if the software that reads them supports this.
#'
#' @keywords internal
analyze.pair.files <- function(filepath, require_header = FALSE) {
  if (!is.character(filepath) || length(filepath) != 1L) {
    stop("Parameter 'filepath' must be a character string.")
  }

  gz_suffix <- ""
  base <- filepath
  if (endsWith(tolower(base), ".gz")) {
    gz_suffix <- substr(base, nchar(base) - 2L, nchar(base))
    base <- substr(base, 1L, nchar(base) - 3L)
  }
  if (endsWith(tolower(base), ".hdr") || endsWith(tolower(base), ".img")) {
    base <- substr(base, 1L, nchar(base) - 4L)
  }

  pair <- list()
  pair$header <- sprintf("%s.hdr%s", base, gz_suffix)
  pair$image <- sprintf("%s.img%s", base, gz_suffix)

  # A pair may also be stored uncompressed under a .gz name and vice versa, but we only report what the name says.
  if (!file.exists(pair$header) && !file.exists(pair$image)) {
    # Fall back to the other combination of the two suffixes, in case the header is compressed and the data is not
    # (or the other way around), which the file names above cannot express.
    if (nchar(gz_suffix) > 0L) {
      if (file.exists(sprintf("%s.hdr", base)) || file.exists(sprintf("%s.img", base))) {
        pair$header <- sprintf("%s.hdr", base)
        pair$image <- sprintf("%s.img", base)
      }
    }
  }

  pair$header_exists <- file.exists(pair$header)
  pair$image_exists <- file.exists(pair$image)

  # SPM and FreeSurfer write the transformation matrix of an ANALYZE image into a MATLAB file with the same base
  # name, as '<base>.mat'. It is not part of the format, but it is the only reliable geometry such a file can have.
  pair$mat <- sprintf("%s.mat", base)
  pair$mat_exists <- file.exists(pair$mat)

  if (require_header && !pair$header_exists) {
    stop(sprintf("ANALYZE/NIFTI pair file '%s' does not exist (expected the header file '%s').\n", filepath, pair$header))
  }

  return(pair)
}


#' @title Read the 4 magic bytes of an ANALYZE 7.5 or NIFTI v1 header.
#'
#' @param filepath character string, the path to the header file of a pair.
#'
#' @return character string, the 4 bytes at offset 344 of the file, interpreted as a string. This is the NIFTI v1
#'   `magic` field, which is empty (`''`) for ANALYZE 7.5 files (where the same bytes are the `smin` field), `'ni1'`
#'   for a NIFTI v1 pair file and `'n+1'` for a single file NIFTI v1 file.
#'
#' @keywords internal
analyze.read.magic <- function(filepath) {
  fh <- fileopen.gz.or.not(filepath)
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )
  num_skip <- 344L # skip everything before the magic field
  discarded <- readBin(fh, integer(), n = num_skip, size = 1L)
  discarded <- NULL
  magic_bytes <- readBin(fh, "raw", n = 4L)
  return(rawToChar(magic_bytes[magic_bytes != as.raw(0)]))
}


#' @title Read ANALYZE 7.5 header from file.
#'
#' @description Read the header of an ANALYZE 7.5 file, i.e. of a file in the two-file format that stores the voxel
#'   data in a separate `.img` file.
#'
#' @param filepath character string, the path to the `.hdr` file. The base name without the extension is accepted as
#'   well, see \code{\link{analyze.pair.files}}.
#'
#' @return named list with the ANALYZE 7.5 header fields, in the field naming of the format specification. See
#'   \code{\link{analyzeheader.template}} for a description of all of them. In addition to the fields of the
#'   specification, the following entries are present:
#'
#' \describe{
#'   \item{`endian`}{character string, the endianness of the file, `'little'` or `'big'`. It is detected from the
#'     `sizeof_hdr` field.}
#'   \item{`magic`}{character string, the 4 bytes at offset 344 (which are the `smin` field of the ANALYZE
#'     specification), interpreted as a string. It is empty for ANALYZE files, see \code{\link{is.analyze.file}}.}
#'   \item{`header_format`}{character string, always `'analyze'` here. The other variant of the two-file format is
#'     reported as `'nifti1_pair'`, see \code{\link{read.fs.volume.analyze}}.}
#'   \item{`spm_origin`}{integer vector of length 3, the interpretation that SPM gives to the `originator` field:
#'     the voxel coordinates of the image origin, as stored in the first 6 bytes of the field. It is `NULL` if all
#'     of them are zero, which means that the field does not store an origin.}
#'   \item{`originator_bytes`}{raw vector of length 10, the unmodified content of the `originator` field. The
#'     `originator` entry is the same content with the zero bytes removed and interpreted as a string, which loses
#'     information for the files in which SPM stores the image origin there.}
#'   \item{`filepath_header`, `filepath_image`}{character strings, the paths of the two files of the pair.}
#' }
#'
#' @note ANALYZE 7.5 does not define the meaning of the world coordinate system of an image: the header stores the
#'   voxel sizes but neither the direction of the voxel axes nor the position of the image in space. This function
#'   therefore reports the fields as they are stored and does not derive a transformation matrix. See
#'   \code{\link{read.fs.volume.analyze}} for the ways to get a matrix anyway, and for what is known and unknown
#'   about them.
#'
#' @seealso \code{\link{read.analyze.data}}, \code{\link{analyzeheader.template}}
#'
#' @examples
#' hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' analyzeheader <- read.analyze.header(hdrfile)
#' analyzeheader$datatype
#' analyzeheader$pix_dim
#'
#' @export
read.analyze.header <- function(filepath) {
  pair <- analyze.pair.files(filepath, require_header = TRUE)
  analyzeheader <- analyze.read.header.internal(pair$header)

  if (analyzeheader$magic %in% c("ni1", "n+1")) {
    stop(sprintf("File '%s' is not an ANALYZE 7.5 file: its header contains the NIFTI v1 magic string '%s'. Use the NIFTI reader for it, i.e. read.nifti1.header('%s') for a NIFTI v1 pair or read.nifti1.header/read.nifti2.header for a single file NIFTI file.\n", filepath, analyzeheader$magic, pair$header))
  }

  analyzeheader$filepath_header <- pair$header
  analyzeheader$filepath_image <- pair$image
  return(analyzeheader)
}


#' @title Read a fixed length character field of an ANALYZE 7.5 header.
#'
#' @param filehandle connection to read from.
#'
#' @param n integer, the number of bytes of the field.
#'
#' @return character string, the field content with the trailing zero bytes removed. The bytes are interpreted as
#'   ISO-8859-1 (latin-1) and converted to UTF-8, which cannot fail: the ANALYZE 7.5 fields `originator`,
#'   `generated`, `patient_id` and friends contain whatever the software that wrote the file put there, including
#'   bytes that are not valid UTF-8 (the SPM software, for example, stores an image origin as 3 little endian
#'   integers in the `originator` field). Decoding them as UTF-8 would fail or return `NA` for a subset of the
#'   possible byte values.
#'
#' @keywords internal
analyze.read.char.field <- function(filehandle, n) {
  field_bytes <- readBin(filehandle, "raw", n)
  field_bytes <- field_bytes[field_bytes != as.raw(0)]
  return(iconv(rawToChar(field_bytes), from = "ISO-8859-1", to = "UTF-8"))
}


#' @title Read ANALYZE 7.5 header from file.
#'
#' @inheritParams read.analyze.header
#'
#' @param little_endian logical, leave this alone. The endianness is detected automatically, and messing with this
#'   parameter only makes the detection report a wrong endianness for a file of the other one.
#'
#' @return named list, the ANALYZE 7.5 header. See \code{\link{read.analyze.header}}.
#'
#' @keywords internal
analyze.read.header.internal <- function(filepath, little_endian = TRUE) {
  endian <- ifelse(little_endian, "little", "big")

  fh <- fileopen.gz.or.not(filepath)
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )

  analyzeheader <- list("endian" = endian)

  analyzeheader$sizeof_hdr <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  if (analyzeheader$sizeof_hdr != 348L) {
    if (little_endian == FALSE) { # if called with FALSE, the TRUE option was already checked.
      if (analyzeheader$sizeof_hdr == 540L) {
        stop("File not in ANALYZE 7.5 format: header size 540 looks like a NIFTI v2 file.")
      }
      stop(sprintf("File '%s' is not in ANALYZE 7.5 format: invalid header size %d, expected 348.\n", filepath, analyzeheader$sizeof_hdr)) # nocov
    } else {
      return(analyze.read.header.internal(filepath, little_endian = FALSE))
    }
  }

  analyzeheader$data_type <- analyze.read.char.field(fh, 10L)
  analyzeheader$db_name <- analyze.read.char.field(fh, 18L)
  analyzeheader$extents <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$session_error <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  analyzeheader$regular <- analyze.read.char.field(fh, 1L)
  analyzeheader$hkey_un0 <- analyze.read.char.field(fh, 1L)

  analyzeheader$dim <- readBin(fh, integer(), n = 8, size = 2, endian = endian)

  analyzeheader$vox_units <- analyze.read.char.field(fh, 4L)
  analyzeheader$cal_units <- analyze.read.char.field(fh, 8L)
  analyzeheader$unused1 <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  analyzeheader$datatype <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  analyzeheader$bitpix <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
  analyzeheader$dim_un0 <- readBin(fh, integer(), n = 1, size = 2, endian = endian)

  analyzeheader$pix_dim <- readBin(fh, numeric(), n = 8, size = 4, endian = endian)
  analyzeheader$vox_offset <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)

  analyzeheader$funused1 <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)
  analyzeheader$funused2 <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)
  analyzeheader$funused3 <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)

  analyzeheader$cal_max <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)
  analyzeheader$cal_min <- readBin(fh, numeric(), n = 1, size = 4, endian = endian)
  analyzeheader$compressed <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$verified <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$glmax <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$glmin <- readBin(fh, integer(), n = 1, size = 4, endian = endian)

  analyzeheader$descrip <- analyze.read.char.field(fh, 80L)
  analyzeheader$aux_file <- analyze.read.char.field(fh, 24L)
  analyzeheader$orient <- readBin(fh, integer(), n = 1, size = 1)

  # The 'originator' field is a char[10] per the specification, but SPM stores the image origin in its first 6
  # bytes, as 3 int16 values. The bytes are read raw to support both interpretations.
  originator_bytes <- readBin(fh, "raw", n = 10L)
  analyzeheader$originator <- analyze.read.char.field(originator_bytes, 10L)
  analyzeheader$originator_bytes <- originator_bytes
  analyzeheader$spm_origin <- NULL
  spm_origin <- readBin(con = originator_bytes[1:6], what = integer(), n = 3, size = 2, endian = endian)
  if (any(spm_origin != 0L)) {
    analyzeheader$spm_origin <- spm_origin
  }

  analyzeheader$generated <- analyze.read.char.field(fh, 10L)
  analyzeheader$scannum <- analyze.read.char.field(fh, 10L)
  analyzeheader$patient_id <- analyze.read.char.field(fh, 10L)
  analyzeheader$exp_date <- analyze.read.char.field(fh, 10L)
  analyzeheader$exp_time <- analyze.read.char.field(fh, 10L)
  analyzeheader$hist_un0 <- analyze.read.char.field(fh, 3L)

  analyzeheader$views <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$vols_added <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$start_field <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$field_skip <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$omax <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$omin <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
  analyzeheader$smax <- readBin(fh, integer(), n = 1, size = 4, endian = endian)

  # The last 4 bytes are 'smin' in ANALYZE and 'magic' in NIFTI.
  smin_bytes <- readBin(fh, "raw", n = 4L)
  analyzeheader$smin <- readBin(con = smin_bytes, what = integer(), n = 1, size = 4, endian = endian)
  analyzeheader$magic <- rawToChar(smin_bytes[smin_bytes != as.raw(0)])
  analyzeheader$header_format <- ifelse(analyzeheader$magic %in% c("ni1", "n+1"), "nifti1_pair", "analyze")

  if (analyzeheader$dim[1] < 0L || analyzeheader$dim[1] > 7L) {
    stop(sprintf("File '%s' is not in ANALYZE 7.5 format: invalid number of dimensions %d in the 'dim' field, expected a value between 0 and 7. This usually means that the file uses a different endianness or is not an image file.\n", filepath, analyzeheader$dim[1]))
  }

  return(analyzeheader)
}


#' @title Read the matrices of a MATLAB v4 data file.
#'
#' @description MATLAB v4 files (the format behind the `.mat` files that SPM and FreeSurfer write next to an
#'   ANALYZE image, see \code{\link{read.fs.volume.analyze}}) are the simplest MATLAB data format: a small header
#'   with the type, the dimensions, the imaginary flag and the variable name, followed by the raw values in column
#'   major order. This function reads the matrices of such a file.
#'
#' @param filepath character string, the path of the file to read.
#'
#' @return named list of matrices, one per variable in the file (a v4 file may contain several variables, which are
#'   stored one after another). The names are the variable names (e.g. `'M'` or `'mat'`), and the values are
#'   matrices of the stored dimensions. Returns `NULL` if the file is not a MATLAB v4 file, i.e. if no interpretation
#'   of its content leads to a file whose size matches the declared variables. Other MATLAB file versions (v5 and
#'   newer, which are compressed and have a completely different structure) are reported as `NULL` as well, instead
#'   of returning data from a wrongly interpreted header.
#'
#' @note Only the numeric types of the format are read (double, float, int32, int16, uint16 and uint8, which are
#'   the type codes 0, 10, 20, 30, 40 and 50). Text matrices and variables with more than 2 dimensions are not
#'   supported, the latter because the v4 format cannot store them.
#'
#' @keywords internal
read.matlab.v4.matrix <- function(filepath) {
  if (!file.exists(filepath)) {
    return(NULL)
  }
  file_size <- file.size(filepath)
  if (is.na(file_size) || file_size < 20L) {
    return(NULL)
  }

  type_sizes <- c("0" = 8L, "10" = 4L, "20" = 4L, "30" = 2L, "40" = 2L, "50" = 1L)

  fh <- file(filepath, "rb")
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )

  # The file is stored in the byte order of the machine that wrote it, which the header does not state. The first
  # variable is therefore parsed with both byte orders, and the one that describes a file of exactly the size of
  # this file is used.
  variables <- NULL
  for (endian in c("little", "big")) {
    seed <- readBin(con = readBin(filepath, "raw", n = 20L), what = integer(), n = 1L, size = 4L, endian = endian)
    if (!(as.character(seed) %in% names(type_sizes))) {
      next
    }
    seek(fh, where = 0L, origin = "start")
    variables <- list()
    valid <- TRUE
    while (valid) {
      position <- seek(fh, where = 0L, origin = "current")
      if (position >= file_size) {
        break
      }
      header_bytes <- readBin(fh, "raw", n = 20L)
      if (length(header_bytes) < 20L) {
        valid <- FALSE
        break
      }
      header <- readBin(con = header_bytes, what = integer(), n = 5L, size = 4L, endian = endian)
      type <- header[1L]
      mrows <- header[2L]
      ncols <- header[3L]
      imagf <- header[4L]
      namelen <- header[5L]
      if (!(as.character(type) %in% names(type_sizes)) || imagf != 0L || mrows < 1L || ncols < 1L || namelen < 1L) {
        valid <- FALSE
        break
      }
      num_values <- mrows * ncols
      if (position + 20L + namelen + num_values * type_sizes[[as.character(type)]] > file_size) {
        valid <- FALSE
        break
      }
      name <- rawToChar(readBin(fh, "raw", n = namelen))
      name_bytes <- as.integer(charToRaw(name))
      # The variable name is zero terminated and padded with zeroes.
      name <- rawToChar(as.raw(name_bytes[name_bytes != 0L]))
      values <- readBin(fh, "numeric", n = num_values, size = type_sizes[[as.character(type)]], endian = endian)
      if (length(values) != num_values) {
        valid <- FALSE
        break
      }
      variables[[name]] <- matrix(values, nrow = mrows, ncol = ncols) # the v4 format stores columns first, like R
      if (position + 20L + namelen + num_values * type_sizes[[as.character(type)]] == file_size) {
        break
      }
    }
    if (!valid || length(variables) == 0L) {
      variables <- NULL
    } else {
      return(variables)
    }
  }

  return(NULL)
}


#' @title Compute the voxel-to-RAS matrix from the MATLAB sidecar file of an ANALYZE image.
#'
#' @description Read the transformation matrix that SPM and FreeSurfer write into the `.mat` file next to an ANALYZE
#'   image, and convert it to the convention used by this package (0-based voxel indices, right-anterior-superior
#'   world coordinates). See the geometry section of \code{\link{read.fs.volume.analyze}} for the details.
#'
#' @param matfile character string, the path of the `.mat` file.
#'
#' @return named list with the entries `vox2ras` (a 4x4 numeric matrix, or `NULL` if the file could not be read or
#'   contains no usable matrix) and `reason` (a character string describing the problem, or `NULL` on success).
#'
#' @keywords internal
analyze.mat.sidecar.to.vox2ras <- function(matfile) {
  variables <- read.matlab.v4.matrix(matfile)
  if (is.null(variables)) {
    return(list("vox2ras" = NULL, "reason" = "the file is not a MATLAB v4 file, or its header could not be interpreted. Only uncompressed v4 files are supported, not the v5 and newer formats that current MATLAB versions write by default"))
  }

  if ("mat" %in% names(variables)) {
    # The 'mat' matrix already contains the orientation flip, so it is used as it is.
    vox2ras <- variables[["mat"]]
  } else if ("M" %in% names(variables)) {
    # The 'M' matrix does not contain the flip of the first axis, which the format's original software applies.
    vox2ras <- diag(c(-1., 1., 1., 1.)) %*% variables[["M"]]
  } else {
    return(list("vox2ras" = NULL, "reason" = sprintf("the file contains no variable named 'mat' or 'M', only %s", paste(sprintf("'%s'", names(variables)), collapse = ", "))))
  }

  if (!is.matrix(vox2ras) || !all(dim(vox2ras) == c(4L, 4L))) {
    return(list("vox2ras" = NULL, "reason" = sprintf("the matrix is not a 4x4 matrix, but has dimensions %s", paste(dim(vox2ras), collapse = "x"))))
  }

  # The matrix maps the 1-based voxel indices that MATLAB uses, so it has to be adjusted for the 0-based indices of
  # this package: for a voxel index v the two are related by A_0based(v) = A_1based(v + 1), which adds the sum of
  # each row of the rotation part to its translation. This is a shift of one voxel, i.e. of several millimeters, and
  # it is what the reference implementation of the format (nibabel) does as well.
  vox2ras[1:3, 4L] <- vox2ras[1:3, 4L] + rowSums(vox2ras[1:3, 1:3])

  if (abs(det(vox2ras[1:3, 1:3])) < 1e-12) {
    return(list("vox2ras" = NULL, "reason" = "the linear part of the matrix is singular"))
  }

  return(list("vox2ras" = vox2ras, "reason" = NULL))
}


#' @title Read ANALYZE 7.5 data from file.
#'
#' @inheritParams read.analyze.header
#'
#' @param header optional ANALYZE 7.5 header as returned by \code{\link{read.analyze.header}}. It will be read from
#'   the file automatically if left at `NULL`.
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions in the returned data array, see
#'   \code{\link{drop}}.
#'
#' @return the data in the `.img` file of the pair, as an array (or a vector for 1D data). Note that the fields of
#'   the header are not applied in any way: the data are returned exactly as they are stored in the file, in the
#'   storage order of the format (the first dimension varies fastest, which is also the order used by R and by the
#'   MGH/MGZ format), and the `funused1` field that SPM uses as a scale factor is not applied (see
#'   \code{\link{read.fs.volume.analyze}} if you want that).
#'
#' @seealso \code{\link{read.analyze.header}}
#'
#' @examples
#' hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' data <- read.analyze.data(hdrfile)
#' dim(data)
#'
#' @export
read.analyze.data <- function(filepath, header = NULL, drop_empty_dims = TRUE) {
  pair <- analyze.pair.files(filepath, require_header = TRUE)
  if (is.null(header)) {
    header <- read.analyze.header(filepath)
  }

  if (!file.exists(pair$image)) {
    stop(sprintf("The data file '%s' of the ANALYZE header file '%s' does not exist.\n", pair$image, pair$header))
  }

  fh <- fileopen.gz.or.not(pair$image)
  on.exit(
    {
      close(fh)
    },
    add = TRUE
  )

  endian <- header$endian

  # ANALYZE has no data offset, the data starts at the first byte of the .img file. Some files store a value in the
  # 'vox_offset' field anyway, but it is unusable: the file needs to be read from the beginning, which is what every
  # implementation of the format does.
  if (!is.null(header$vox_offset) && header$vox_offset != 0) {
    warning(sprintf("The header of ANALYZE file '%s' has a non-zero 'vox_offset' field (%.1f). This field is meaningless for ANALYZE files, the data is read from the beginning of the '.img' file.\n", filepath, header$vox_offset))
  }

  data_dim <- nifti.datadim.from.dimfield(header$dim)
  num_values <- prod(data_dim)

  type_info <- analyze.dtype.info(header, filepath)
  read_size_bytes <- type_info$num_bytes

  # Security: validate allocation size before reading data
  validate_allocation_size(data_dim, read_size_bytes)

  data <- read.nifti.values(fh, type_info$datatype, type_info$bitpix, num_values, endian)
  data <- array(data, dim = data_dim)
  if (drop_empty_dims) {
    return(drop(data))
  }
  return(data)
}


#' @title Compute the R data type and the number of bytes per value for an ANALYZE data type.
#'
#' @param analyzeheader named list, an ANALYZE 7.5 header as returned by \code{\link{read.analyze.header}}.
#'
#' @param filepath character string, the path of the file the header was read from. Only used for warning and error
#'   messages.
#'
#' @return named list with the entries `datatype` and `bitpix` (the data type fields to use for reading the data,
#'   which may differ from the values in the file if the file contradicts itself), `num_bytes` (the number of bytes
#'   per value) and `r_dtype` (the R type to read the values as).
#'
#' @note ANALYZE files in the wild sometimes have a wrong `bitpix` field, since the format was used by many tools
#'   that did not care about it. The data type is the reliable field, so if the two fields contradict each other, the
#'   `bitpix` value that belongs to the data type is used, with a warning.
#'
#' @keywords internal
analyze.dtype.info <- function(analyzeheader, filepath = NULL) {
  datatype <- analyzeheader$datatype
  bitpix <- analyzeheader$bitpix

  # The canonical number of bits per value for each ANALYZE/NIFTI data type code.
  canonical_bitpix <- c("2" = 8L, "4" = 16L, "8" = 32L, "16" = 32L, "32" = 64L, "64" = 64L, "128" = 24L, "256" = 8L, "512" = 16L, "768" = 32L, "1024" = 64L, "1280" = 64L)
  num_bytes <- bitpix / 8L

  if (is.null(bitpix) || bitpix <= 0L || !(as.character(datatype) %in% names(canonical_bitpix))) {
    if (datatype == 0L) {
      stop(sprintf("ANALYZE file '%s' has data type 0 ('unknown'), the data cannot be read.\n", filepath))
    }
    if (datatype == 1L) {
      stop(sprintf("ANALYZE file '%s' has data type 1 ('binary'), i.e. one bit per voxel. This data type is not supported.\n", filepath))
    }
    stop(sprintf("ANALYZE file '%s' has unsupported data type %d (with %d bits per value).\n", filepath, datatype, bitpix))
  }

  canonical <- canonical_bitpix[[as.character(datatype)]]
  if (bitpix != canonical) {
    warning(sprintf("The header of ANALYZE file '%s' states %d bits per value, but the data type %d requires %d bits. Using %d bits, as the data type field is the more reliable one (this is what the nibabel implementation of the format does as well).\n", filepath, bitpix, datatype, canonical, canonical))
    num_bytes <- canonical / 8L
    bitpix <- canonical
  }

  dti <- tryCatch(
    {
      nifti.dtype.info(datatype, canonical)
    },
    error = function(e) {
      stop(sprintf("ANALYZE file '%s' has unsupported data type %d (%d bits per value). Supported types are unsigned 8 bit integers, signed 16/32 bit integers and 32/64 bit floating point data; the complex and RGB data types of the format are not supported.\n", filepath, datatype, bitpix))
    }
  )

  return(list("datatype" = datatype, "bitpix" = bitpix, "num_bytes" = num_bytes, "r_dtype" = dti$r_dtype))
}


#' @title Read an ANALYZE 7.5 or NIFTI v1 pair file as an fs.volume.
#'
#' @description Read a volume from the two-file image formats ANALYZE 7.5 and NIFTI v1: a 348 byte header
#'   (`<base>.hdr`) and the voxel data in a separate file (`<base>.img`). Read
#'   \code{\link{read.analyze.header}} for the difference between the two variants and for the limitations of the
#'   ANALYZE header.
#'
#' @param filepath character string, the path to one of the two files of the pair, or to its base name. See
#'   \code{\link{analyze.pair.files}}.
#'
#' @param spm logical, whether to interpret the ANALYZE header the way SPM, FreeSurfer and the other tools of the
#'   ecosystem do. This concerns the fields that ANALYZE 7.5 leaves undefined but that SPM uses: `funused1` is the
#'   scale factor of the data, and `originator` holds the voxel coordinates of the image origin. It makes this
#'   function apply the scale factor, and use the origin to derive a transformation matrix when the file has no
#'   MATLAB sidecar file (see the geometry section below). Only relevant for ANALYZE files, since the NIFTI v1
#'   variant of the pair stores a proper transformation matrix.
#'
#' @inheritParams read.fs.mgh
#'
#' @return an `fs.volume` instance, i.e. a named list with the entries `data` (the voxel data) and `header` (the
#'   image header), or only the data array if `with_header` is `FALSE`. The header is the format specific header (an
#'   ANALYZE 7.5 header as returned by \code{\link{read.analyze.header}} for ANALYZE files, a NIFTI v1 header as
#'   returned by \code{\link{read.nifti1.header}} for NIFTI pair files), and not an MGH header. In addition, the
#'   following entries are present:
#'
#' \describe{
#'   \item{`vox2ras_matrix`}{the 4x4 voxel-to-RAS transformation matrix, computed from the `sform`/`qform` header
#'     fields for NIFTI v1 pair files. For ANALYZE files this entry is only present if `spm` is `TRUE`, see the
#'     note below.}
#'   \item{`vox2ras_source`}{character string, a description of where the matrix comes from, one of `'sform'`,
#'     `'qform'` (for NIFTI v1 pair files), `'mat sidecar'`, `'spm origin'` or `'analyze convention'` (for ANALYZE
#'     files, see the geometry section). Only present if `vox2ras_matrix` is present.}
#'   \item{`voldim`}{only present if `flatten` is `TRUE`, exactly as in the other volume readers.}
#'   \item{`filepath`, `filepath_header`, `filepath_image`}{the paths of the header and data files.}
#' }
#'
#' @section The geometry of ANALYZE 7.5 files:
#' The ANALYZE 7.5 header stores the voxel sizes (`pix_dim`) but neither the direction of the voxel axes in world
#' space nor the position of the image in it. This means that the left/right orientation of an ANALYZE image is
#' *not known* from the header, and no implementation of the format can know it: the format is famous for exactly
#' this problem, and the `orient` field that was added to fix it is set by some tools and ignored by others.
#'
#' There are two ways to get a transformation matrix for such a file, and this function supports both:
#'
#' \describe{
#'   \item{The MATLAB sidecar file}{SPM and FreeSurfer write the transformation matrix into a file named
#'     `<base>.mat` next to the `<base>.img` file, as a MATLAB v4 file with a variable named `mat` or `M`. This
#'     matrix is real information from the file set, not a convention, so it is used whenever such a file is
#'     present, and the `vox2ras_source` of the result is `'mat sidecar'`. The `M` variant does not contain the
#'     flip of the first voxel axis that the format's original software applies, so that flip is added (this is
#'     also what the reference implementation of the format, nibabel, does). Note that the matrix in the file maps
#'     the *1-based* voxel indices that MATLAB uses, and that the matrix returned here maps the 0-based indices
#'     that R and this package use: the difference is the sum of the rows of the rotation part, i.e. up to several
#'     millimeters, and getting it wrong is exactly the kind of silent error that is hard to notice. FreeSurfer
#'     writes only the `M` variable, nibabel writes both. MATLAB files of version 5 and newer (the `.mat` files
#'     that recent MATLAB versions write, which are compressed) cannot be read; such a file is reported and
#'     ignored, the volume data is still returned.}
#'   \item{The SPM convention (`spm = TRUE`)}{If there is no sidecar file, the SPM and FreeSurfer tools derive the
#'     geometry from the fields that ANALYZE leaves unused: the `originator` field holds the voxel coordinates of
#'     the image origin, the voxel sizes give the axis lengths, and the axes are assumed to point to the left, the
#'     front and the top (`diag(-x, y, z)`, the convention of the format's original software). The same `spm =
#'     TRUE` applies the scale factor that SPM stores in `funused1`. Both are heuristics that the file does not
#'     state, which is why they are not used by default: a matrix from a convention can be wrong by a mirror
#'     image.}
#' }
#'
#' Without a sidecar file and with `spm = FALSE` (the default), no matrix is reported at all: the voxel sizes and
#' the orientation code are returned as they are stored, and you can build a matrix from them if the convention of
#' your choice is known to be the right one for your data.
#'
#' @seealso \code{\link{read.analyze.header}}, \code{\link{read.analyze.data}}, \code{\link{read.fs.volume}}
#'
#' @examples
#' hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' vol <- read.fs.volume.analyze(hdrfile, with_header = TRUE)
#' dim(vol$data)
#' vol$header$pix_dim
#'
#' @export
read.fs.volume.analyze <- function(filepath, flatten = FALSE, with_header = FALSE, drop_empty_dims = FALSE, spm = FALSE) {
  pair <- analyze.pair.files(filepath, require_header = TRUE)

  if (!pair$image_exists) {
    stop(sprintf("The data file '%s' of the ANALYZE/NIFTI pair '%s' does not exist.\n", pair$image, pair$header))
  }

  analyzeheader <- analyze.read.header.internal(pair$header)
  vox2ras <- NULL
  vox2ras_source <- NULL

  if (analyzeheader$magic == "ni1") {
    # A NIFTI v1 pair file: the header is a proper NIFTI v1 header, including the geometry fields.
    header <- read.nifti1.header(pair$header)
    values <- read.nifti1.data(pair$header, header = header, drop_empty_dims = FALSE)
    vox2ras <- nifti.header.to.vox2ras(header)
    if (!is.null(vox2ras)) {
      vox2ras_source <- ifelse(header$sform_code != 0L, "sform", "qform")
    }
    if (spm) {
      warning("The parameter 'spm' is only used for ANALYZE 7.5 files: this file is a NIFTI v1 pair, which stores its own transformation matrix. The parameter is ignored.\n")
    }
  } else if (analyzeheader$magic == "n+1") {
    stop(sprintf("File '%s' is a single file NIFTI v1 file, which does not have a separate data file. Use read.fs.volume.nii() or read.fs.volume() to read it.\n", pair$header))
  } else {
    header <- read.analyze.header(filepath)
    values <- read.analyze.data(filepath, header = header, drop_empty_dims = FALSE)

    # The MATLAB sidecar file with the transformation matrix, which SPM and FreeSurfer write next to the image. It
    # is not part of the format, but it is real information from the file set (and the most reliable geometry that
    # an ANALYZE image can have), so it is used by default, and it takes precedence over the SPM origin.
    if (pair$mat_exists) {
      mat_result <- analyze.mat.sidecar.to.vox2ras(pair$mat)
      if (is.null(mat_result$vox2ras)) {
        warning(sprintf("The MATLAB sidecar file '%s' that belongs to ANALYZE file '%s' was ignored, because %s. The volume data is read as usual; only the transformation matrix is missing.\n", pair$mat, filepath, mat_result$reason))
      } else {
        vox2ras <- mat_result$vox2ras
        vox2ras_source <- "mat sidecar"
      }
    }

    scale_applied <- FALSE
    if (spm && header$funused1 != 0. && header$funused1 != 1.) {
      values <- values * header$funused1
      header$data_scaled_by <- header$funused1
      scale_applied <- TRUE
    }

    if (is.null(vox2ras) && spm) {
      # SPM and FreeSurfer derive the geometry from the image origin when there is no sidecar file, and this is
      # also the convention that the reference implementation of the format uses for these files, see the
      # documentation of this function.
      vox2ras <- analyze.header.to.vox2ras(header)
      vox2ras_source <- ifelse(is.null(header$spm_origin), "analyze convention", "spm origin")
    }

    # Report the fields that use space the ANALYZE specification leaves unused, and that are ignored.
    ignored_spm_fields <- character(0)
    if (!scale_applied && header$funused1 != 0. && header$funused1 != 1.) {
      ignored_spm_fields <- c(ignored_spm_fields, sprintf("the 'funused1' field holds the SPM data scale factor (%.4f)", header$funused1))
    }
    if (is.null(vox2ras) && !is.null(header$spm_origin)) {
      ignored_spm_fields <- c(ignored_spm_fields, sprintf("the 'originator' field holds the SPM image origin (%s)", paste(header$spm_origin, collapse = " ")))
    }
    if (length(ignored_spm_fields) > 0L) {
      warning(sprintf("ANALYZE file '%s' contains data that ANALYZE 7.5 does not define, but that SPM and FreeSurfer store in the same header fields: %s. It is ignored. Pass 'spm = TRUE' to use it, see the documentation of this function.\n", filepath, paste(ignored_spm_fields, collapse = ", and ")))
    }
  }

  if (drop_empty_dims) {
    values <- drop(values)
  }

  data <- array(values, dim = dim(values))
  if (flatten) {
    dim(data) <- c(length(values))
    data <- as.vector(unlist(data))
    header$voldim <- c(length(data))
  }

  header$filepath <- filepath
  header$filepath_header <- pair$header
  header$filepath_image <- pair$image
  if (!is.null(vox2ras)) {
    header$vox2ras_matrix <- vox2ras
    header$vox2ras_source <- vox2ras_source
  }

  if (with_header) {
    return_list <- list()
    return_list$header <- header
    return_list$data <- data
    class(return_list) <- "fs.volume"
    return(return_list)
  }
  return(data)
}


#' @title Compute a transformation matrix from the fields of an ANALYZE 7.5 header.
#'
#' @description Compute the voxel-to-RAS matrix for an ANALYZE 7.5 image, using the conventions of SPM and of the
#'   original ANALYZE software. See the "geometry" section of \code{\link{read.fs.volume.analyze}} for what this
#'   matrix is and is not.
#'
#' @param analyzeheader named list, an ANALYZE 7.5 header as returned by \code{\link{read.analyze.header}}.
#'
#' @return a 4x4 numeric matrix, the voxel-to-RAS transformation matrix. The matrix assumes that the first voxel
#'   axis points to the left (i.e. it contains `-pix_dim[2]` in its first column), which is the convention of the
#'   format. If the SPM origin is present in the header, the matrix maps the origin to the world position that the
#'   SPM fields describe, otherwise the center of the image is used as the origin, exactly as the reference
#'   implementation of the format (nibabel) does it.
#'
#' @keywords internal
analyze.header.to.vox2ras <- function(analyzeheader) {
  zooms <- as.numeric(analyzeheader$pix_dim[2:4])
  zooms[1] <- -zooms[1] # the x axis flip convention of the format

  dims <- as.numeric(nifti.datadim.from.dimfield(analyzeheader$dim))[1:3]

  if (!is.null(analyzeheader$spm_origin) && any(analyzeheader$spm_origin != 0L)) {
    origin <- as.numeric(analyzeheader$spm_origin)
    # The SPM origin is stored with 1-based MATLAB indexing, and only used if it is within a sane range.
    if (all(origin > -dims) && all(origin < dims * 2)) {
      origin <- origin - 1
    } else {
      origin <- (dims - 1) / 2.
    }
  } else {
    origin <- (dims - 1) / 2.
  }

  vox2ras <- diag(4L)
  vox2ras[1:3, 1:3] <- diag(zooms)
  vox2ras[1:3, 4L] <- -origin * zooms

  return(vox2ras)
}
