# Read the header and data parts of NIFTI v1 files. These files may or may not have the FreeSurfer NIFTI hack.


#' @title Read NIFTI v1 header from file (which may contain the FreeSurfer hack).
#'
#' @param filepath path to a NIFTI v1 file (single file version), which can contain the FreeSurfer hack.
#'
#' @return named list with NIFTI 1 header fields.
#'
#' @note The FreeSurfer hack is a non-standard way to save long vectors (one dimension greater than 32767 entries) in NIFTI v1 files. Files with this hack are produced when converting MGH or MGZ files containing such long vectors with the FreeSurfer 'mri_convert' tool.
#'
#' @export
read.nifti1.header <- function(filepath) {
  return(read.nifti1.header.internal(filepath, little_endian = TRUE));
}


#' @title Determine whether a NIFTI file uses the FreeSurfer hack.
#'
#' @inheritParams read.nifti1.header
#'
#' @return logical, whether the file header contains the FreeSurfer format hack. See \code{\link{read.nifti1.header}} for details. This function detects NIFTI v2 files, but as they cannot contain the hack, it will always return `FALSE` for them.
#'
#' @note Applying this function to files which are not in NIFTI format will result in an error. See \code{\link{nifti.file.version}} to determine whether a file is a NIFTI file.
#'
#' @export
nifti.file.uses.fshack <- function(filepath) {
  nv = nifti.file.version(filepath);
  if(nv == 1L) {
    nh = read.nifti1.header(filepath);
    return(nh$uses_freesurfer_hack);
  } else if(nv == 2L) {
    return(FALSE);
  } else {
    stop("Not a NIFTI v1 or v2 file.");
  }
}


#' @title Read NIFTI v1 header from file (which may contain the FreeSurfer hack).
#'
#' @inheritParams read.nifti1.header
#'
#' @param little_endian internal logical, leave this alone. Endianness will be figured out automatically, messing with this parameter only hurts.
#'
#' @return named list with NIFTI 1 header fields.
#'
#' @keywords internal
read.nifti1.header.internal <- function(filepath, little_endian = TRUE) {

  endian = ifelse(little_endian, "little", 'big');
  niiheader = list('endian' = endian);

  fh = fileopen.gz.or.not(filepath);
  on.exit({ close(fh) }, add=TRUE);

  niiheader$sizeof_hdr = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  if(niiheader$sizeof_hdr != 348L) {
    if(niiheader$sizeof_hdr == 540L) {
      stop("File not in NIFTI 1 format: header size 540 looks like a NIFTI v2 file.");
    }
    if(little_endian == FALSE) { # if called with FALSE, the TRUE option was already checked.
      stop(sprintf("File not in NIFTI 1 format: invalid header size %d, expected 348.\n", niiheader$sizeof_hdr)); # nocov
    } else {
      return(read.nifti1.header.internal(filepath, little_endian = FALSE));
    }
  }

  num_skip = 36L; # bytes to skip, this part is not used. It exists for compatibility with ANALYZE format.
  discarded = readBin(fh, integer(), n = num_skip, size = 1L);
  discarded = NULL;

  niiheader$dim = readBin(fh, integer(), n = 8, size = 2, endian = endian);
  niiheader$uses_freesurfer_hack = ifelse(niiheader$dim[2] == -1L, TRUE, FALSE);

  niiheader$intent_p1 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$intent_p2 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$intent_p3 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$intent_code = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$datatype = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$bitpix = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$slice_start = readBin(fh, integer(), n = 1, size = 2, endian = endian);

  niiheader$pix_dim = readBin(fh, numeric(), n = 8, size = 4, endian = endian);
  niiheader$vox_offset = readBin(fh, numeric(), n = 1, size = 4, endian = endian); # int would make more sense, but the standard says float
  niiheader$scl_slope = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$scl_inter = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$slice_end = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$slice_code = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  niiheader$xyzt_units = readBin(fh, integer(), n = 1, size = 1, endian = endian);

  niiheader$cal_max = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$cal_min = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$slice_duration = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$toffset = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$glmax = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  niiheader$glmin = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  niiheader$description = readBin(fh, character(), n = 1, endian = endian); # 80 bytes
  niiheader$aux_file = readBin(fh, character(), n = 1, endian = endian); # 24 bytes

  niiheader$qform_code = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$sform_code = readBin(fh, integer(), n = 1, size = 2, endian = endian);

  niiheader$quatern_b = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$quatern_c = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$quatern_d = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$qoffset_x = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$qoffset_y = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$qoffset_z = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$srow_x = readBin(fh, numeric(), n = 4, size = 4, endian = endian);
  niiheader$srow_y = readBin(fh, numeric(), n = 4, size = 4, endian = endian);
  niiheader$srow_z = readBin(fh, numeric(), n = 4, size = 4, endian = endian);

  niiheader$intent_name = readBin(fh, character(), n = 1, endian = endian); # 16 bytes
  niiheader$magic = readBin(fh, character(), n = 1, endian = endian); # 4 bytes

  if(niiheader$uses_freesurfer_hack) { # extract the proper data dimensions from the glmin field. The original value is still available in dim_raw.
    niiheader$dim_raw = niiheader$dim; # only differ for FreeSurfer hack files.
    niiheader$dim[2] = niiheader$glmin;
  }
  return(niiheader);
}


#' @title Determine NIFTI file version information and whether file is a NIFTI file.
#'
#' @param filepath path to a file in NIFTI v1 or v2 format.
#'
#' @return integer, the NIFTI file version. One if `1` for NIFTI v1 files, `2` for NIFTI v2 files, or `NULL` if the file is not a NIFTI file.
#'
#' @export
nifti.file.version <- function(filepath) {
  fh = fileopen.gz.or.not(filepath);

  sizeof_hdr = readBin(fh, integer(), n = 1, size = 4, endian = 'little');
  if(! sizeof_hdr %in% c(348L, 540L)) {
    close(fh);
    fh = fileopen.gz.or.not(filepath);
    sizeof_hdr = readBin(fh, integer(), n = 1, size = 4, endian = 'big');
  }
  close(fh);


  if(sizeof_hdr == 540L) {
    return(2L);
  } else if(sizeof_hdr == 348L) {
    return(1L)
  } else {
    return(NULL);
  }
}


#' @title Get connection to a binary file, gz or not.
#'
#' @param filepath path to the binary file.
#'
#' @keywords internal
fileopen.gz.or.not <- function(filepath) {
  if (endsWith(filepath, '.gz')) {
    fh = gzfile(filepath, "rb");
  }
  else {
    fh = file(filepath, "rb");
  }
  return(fh);
}


#' @title Read raw NIFTI v1 data from file (which may contain the FreeSurfer hack).
#'
#' @inheritParams read.nifti1.header
#'
#' @param header optional nifti header obtained from \code{\link{read.nifti1.header}}. Will be loaded automatically if left at `NULL`.
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions in the loaded data array.
#'
#' @note The FreeSurfer hack is a non-standard way to save long vectors (one dimension greater than 32k entries) in NIFTI v1 files. Files with this hack are produced when converting MGH or MGZ files containing such long vectors with the FreeSurfer 'mri_convert' tool.
#'
#' @return the data in the NIFTI v1 file. Note that the NIFTI v1 header information (scaling, units, etc.) is not applied in any way: the data are returned raw, as read from the file. The information in the header is used to read the data with the proper data type and size.
#'
#' @export
read.nifti1.data <- function(filepath, drop_empty_dims = TRUE, header = NULL) {
  if(is.null(header)) {
    header = read.nifti1.header(filepath);
  }

  fh = fileopen.gz.or.not(filepath);
  on.exit({ close(fh) }, add=TRUE);

  endian = header$endian;

  # move to data part
  num_skip = header$vox_offset;
  discarded = readBin(fh, integer(), n = num_skip, size = 1L, endian = endian);
  discarded = NULL;

  data_dim = nifti.datadim.from.dimfield(header$dim);
  num_values = prod(data_dim);

  read_size_bytes = header$bitpix / 8L; # bitpix is the size in bits, but we need bytes.
  dti = nifti.dtype.info(header$datatype, header$bitpix);

  data = readBin(fh, dti$r_dtype, n = num_values, size = read_size_bytes, endian = endian);
  data = array(data, dim = data_dim);
  if(drop_empty_dims) {
    return(drop(data));
  }
  return(data);
}
