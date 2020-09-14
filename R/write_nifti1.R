

#' @title Create a template NIFTI v1 header. You will have to adapt it for your use case.
#'
#' @return named list, the NIFTI v1 header. All fields are present and filled with values of a proper type. Whether or not they make sense is up to you, but you will most likely have to adapt at least the following fields to your data: `dim_raw`, `datatype`, `bitpix`.
#'
#' @note Commonly used data type settings are: for signed integers datatype = `8L` and bitpix = `32L`; for floats datatype = `16L` and bitpix = `32L`. See the NIFTI v1 standard for more options.
#'
#' @export
ni1header.template <- function() {
  niiheader = list('endian' = 'little');

  niiheader$sizeof_hdr = 348L;

  niiheader$dim = c(3, 256, 256, 256, 1, 1, 1, 1);
  niiheader$dim_raw = niiheader$dim;

  niiheader$intent_p1 = 0.0;
  niiheader$intent_p2 = 0.0;
  niiheader$intent_p3 = 0.0;

  niiheader$intent_code = 0L;
  niiheader$datatype = 16L;
  niiheader$bitpix = 4 * 8L;
  niiheader$slice_start = 0L;

  niiheader$pix_dim = rep(0.0, 8L);
  niiheader$vox_offset = 352.;
  niiheader$scl_slope = 0.0;
  niiheader$scl_inter = 0.0;

  niiheader$slice_end = 0L;
  niiheader$slice_code = 0L;
  niiheader$xyzt_units = 0L;

  niiheader$cal_max = 255.0;
  niiheader$cal_min = 0.0;
  niiheader$slice_duration = 0.0;
  niiheader$toffset = 0.0;

  niiheader$glmax = 0L;
  niiheader$glmin = 0L;

  niiheader$description = 'nifti1file';    # max 80 bytes
  niiheader$aux_file = '';                 # max 24 bytes

  niiheader$qform_code = 0L;
  niiheader$sform_code = 0L;

  niiheader$quatern_b = rep(0.0, 4L);
  niiheader$quatern_c = rep(0.0, 4L);
  niiheader$quatern_d = rep(0.0, 4L);

  niiheader$qoffset_x = rep(0.0, 4L);
  niiheader$qoffset_y = rep(0.0, 4L);
  niiheader$qoffset_z = rep(0.0, 4L);

  niiheader$srow_x = rep(0.0, 4L);
  niiheader$srow_y = rep(0.0, 4L);
  niiheader$srow_z = rep(0.0, 4L);

  niiheader$intent_name = '';     # max 16 bytes
  niiheader$magic = 'n+1';        # max 4 bytes

  return(niiheader);
}


#' @title Create NIFTI v1 header suitable for given data.
#'
#' @param niidata array of numeric (integer or double) data, can have up to 7 dimensions.
#'
#' @param allow_fshack logical, whether to allow data in which the first dimension is larger than 32767, and use the FreeSurfer NIFTI v1 hack to support his. The hack will be used only if needed. WARNING: Files written with the hack do not conform to the NIFTI v1 standard and will not be read correctly by most software. All FreeSurfer tools and the Python 'nibabel' module support it.
#'
#' @return a NIFTI v1 header (see \code{\link{ni1header.template}}) in which the datatype, bitpix, dim and dim_raw fields have been set to values suitable for the given data. Feel free to change the other fields.
ni1header.for.data <- function(niidata, allow_fshack = FALSE) {
  niiheader = ni1header.template();

  if(is.integer(niidata)) {
    niiheader$datatype = 8L;
    niiheader$bitpix = 32L;
  } else if(is.double(niidata)) {
    niiheader$datatype = 16L;
    niiheader$bitpix = 32L;
  } else {
    stop('Only integer or double data is supported by this function.');
  }

  dd = dim(niidata);
  niiheader$dim = dd;
  niiheader$dim_raw = nifti.datadim.to.dimfield(dd);
  niiheader$cal_min = min(niidata);
  niiheader$cal_max = max(niidata);

  nii1_max_vox = 32767L;

  if(dd[1] > nii1_max_vox) {
    if(allow_fshack) {
      niiheader$glmin = dd[1];
      niiheader$dim_raw[2] = -1L;
    } else {
      stop(sprintf("Data dimension #1: %d too large for NIFTI v1 without FreeSurfer hack, limit is %d.\n", dd[1], nii1_max_vox));
    }
  }

  if(any(niiheader$dim_raw > nii1_max_vox)) {
    stop("Data dimensions too large for NIFTI v1 format, consider using NIFTI v2.");
  }

  return(niiheader);
}


#' @title Write header and data to a file in NIFTI v1 format.
#'
#' @param filepath the file to write. The extension should be '.nii' or '.nii.gz'.
#'
#' @param niidata array of numeric or integer data, with up to 7 dimensions. Will be written to the file with the datatype and bitpix specified in the 'niiheader' argument.
#'
#' @param niiheader an optional NIFTI v1 header that is suitable for the passed 'niidata'. If not given, one will be generated with \code{\link{ni1header.for.data}}.
#'
#' @export
write.nifti1 <- function(filepath, niidata, niiheader = NULL) {

  if(is.null(niiheader)) {
    niiheader = ni1header.for.data(niidata);
  }

  if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  endian = niiheader$endian;

  writeBin(as.integer(niiheader$sizeof_hdr), fh, size = 4L, endian = endian);
  writeBin(as.integer(rep(0L, 36L)), fh, size = 1L, endian = endian); # Unused header part, for compatibility with old formats like ANALYZE. We fill it with zeroes.
  writeBin(as.integer(niiheader$dim_raw), fh, size = 2L, endian = endian);

  writeBin(as.double(niiheader$intent_p1), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$intent_p2), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$intent_p3), fh, size = 4L, endian = endian);

  writeBin(as.integer(niiheader$intent_code), fh, size = 2L, endian = endian);
  writeBin(as.integer(niiheader$datatype), fh, size = 2L, endian = endian);
  writeBin(as.integer(niiheader$bitpix), fh, size = 2L, endian = endian);
  writeBin(as.integer(niiheader$sclice_start), fh, size = 2L, endian = endian);

  writeBin(as.double(niiheader$pix_dim), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$vox_offset), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$scl_slope), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$scl_inter), fh, size = 4L, endian = endian);

  writeBin(as.integer(niiheader$sclice_end), fh, size = 2L, endian = endian);
  writeBin(as.integer(niiheader$sclice_code), fh, size = 1L, endian = endian);
  writeBin(as.integer(niiheader$xyzt_units), fh, size = 1L, endian = endian);

  writeBin(as.double(niiheader$cal_max), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$cal_min), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$slice_duration), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$toffset), fh, size = 4L, endian = endian);

  writeBin(as.integer(niiheader$glmax), fh, size = 4L, endian = endian);
  writeBin(as.integer(niiheader$glmin), fh, size = 4L, endian = endian);

  writeChar(niiheader$description, fh, nchars = 80L);
  writeChar(niiheader$aux_file, fh, nchars = 24L);

  writeBin(as.integer(niiheader$qform_code), fh, size = 2L, endian = endian);
  writeBin(as.integer(niiheader$sform_code), fh, size = 2L, endian = endian);

  writeBin(as.double(niiheader$quatern_b), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$quatern_c), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$quatern_d), fh, size = 4L, endian = endian);

  writeBin(as.double(niiheader$qoffset_x), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$qoffset_y), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$qoffset_z), fh, size = 4L, endian = endian);

  writeBin(as.double(niiheader$srow_x), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$srow_y), fh, size = 4L, endian = endian);
  writeBin(as.double(niiheader$srow_z), fh, size = 4L, endian = endian);

  writeChar(niiheader$intent_name, fh, nchars = 16L);
  writeChar(niiheader$magic, fh, nchars = 4L);

  # add zero padding up to 'vox_offset'.
  position_now = 348L;
  num_to_fill = as.integer(niiheader$vox_offset) - position_now;
  writeBin(as.integer(rep(0L, num_to_fill)), fh, size = 1L, endian = endian);

  # Write data.
  if(as.integer(niiheader$datatype) %in% c(2L, 4L, 8L, 512L, 768L)) {  # integer NIFTI data types
    if(! is.integer(niidata)) {
      warning("Found NIFTI integer datatype '%d' in niiheader, but niidata datatype is not integer. Converting data to integer as specified in header.\n", niiheader$datatype);
    }
    data_written = as.integer(t(niidata));
    writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian);
  } else { # treat as double
    if(! is.double(niidata)) {
      warning("Found NIFTI floating point datatype '%d' in niiheader, but niidata datatype is not floating point. Converting data to float as specified in header.\n", niiheader$datatype);
    }
    data_written = as.double(t(niidata));
    writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian);
  }
  close(fh);
  return(invisible(list('header'=niiheader, 'data'=data_written)));
}

