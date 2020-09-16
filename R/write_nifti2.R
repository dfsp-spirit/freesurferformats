

#' @title Create a template NIFTI v2 header. You will have to adapt it for your use case.
#'
#' @return named list, the NIFTI v2 header. All fields are present and filled with values of a proper type. Whether or not they make sense is up to you, but you will most likely have to adapt at least the following fields to your data: `dim_raw`, `datatype`, `bitpix`.
#'
#' @note Commonly used data type settings are: for signed integers datatype = `8L` and bitpix = `32L`; for floats datatype = `16L` and bitpix = `32L`. See the NIFTI v2 standard for more options. You may want to call \code{\link{ni2header.for.data}} instead of this function.
#'
#' @seealso \code{\link{ni2header.for.data}}
#'
#' @export
ni2header.template <- function() {
  niiheader = list('endian' = 'little');

  niiheader$sizeof_hdr = 540L;

  niiheader$dim = c(3, 256, 256, 256, 1, 1, 1, 1);

  niiheader$intent_p1 = 0.0;
  niiheader$intent_p2 = 0.0;
  niiheader$intent_p3 = 0.0;

  niiheader$intent_code = 0L;
  niiheader$datatype = 16L;
  niiheader$bitpix = 4 * 8L;
  niiheader$slice_start = 0L;

  niiheader$pix_dim = rep(0.0, 8L);
  niiheader$vox_offset = 544L;
  niiheader$scl_slope = 0.0;
  niiheader$scl_inter = 0.0;

  niiheader$slice_end = 0L;
  niiheader$slice_code = 0L;
  niiheader$xyzt_units = 0L;

  niiheader$cal_max = 255.0;
  niiheader$cal_min = 0.0;
  niiheader$slice_duration = 0.0;
  niiheader$toffset = 0.0;

  niiheader$descrip = 'nifti2file';    # max 80 bytes
  niiheader$aux_file = '';                 # max 24 bytes

  niiheader$qform_code = 0L;
  niiheader$sform_code = 0L;

  niiheader$quatern_b = 0.0;
  niiheader$quatern_c = 0.0;
  niiheader$quatern_d = 0.0;

  niiheader$qoffset_x = 0.0;
  niiheader$qoffset_y = 0.0;
  niiheader$qoffset_z = 0.0;

  niiheader$srow_x = rep(0.0, 4L);
  niiheader$srow_y = rep(0.0, 4L);
  niiheader$srow_z = rep(0.0, 4L);

  niiheader$intent_name = '';     # max 16 bytes
  niiheader$magic = 'n+1';        # max 4 bytes

  niiheader$dim_info = 0L;

  return(niiheader);
}


#' @title Create NIFTI v2 header suitable for given data.
#'
#' @param niidata array of numeric (integer or double) data, can have up to 7 dimensions.
#'
#' @return a NIFTI v2 header (see \code{\link{ni2header.template}}) in which the datatype, bitpix, dim and dim_raw fields have been set to values suitable for the given data. Feel free to change the other fields.
ni2header.for.data <- function(niidata) {
  niiheader = ni2header.template();

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
  niiheader$dim = nifti.datadim.to.dimfield(dd);
  niiheader$cal_min = min(niidata);
  niiheader$cal_max = max(niidata);

  nifti.header.check(niiheader, nifti_version = 2L);
  return(niiheader);
}




#' @title Write header and data to a file in NIFTI v2 format.
#'
#' @param filepath the file to write. The extension should be '.nii' or '.nii.gz'.
#'
#' @param niidata array of numeric or integer data, with up to 7 dimensions. Will be written to the file with the datatype and bitpix specified in the 'niiheader' argument.
#'
#' @param niiheader an optional NIFTI v2 header that is suitable for the passed 'niidata'. If not given, one will be generated with \code{\link{ni2header.for.data}}.
#'
#' @export
write.nifti2 <- function(filepath, niidata, niiheader = NULL) {

  if(is.null(niiheader)) {
    niiheader = ni2header.for.data(niidata);
  }

  if(! nifti.header.check(niiheader, nifti_version = 2L)) {
    stop("Invalid NIFTI v2 header.");
  }

  if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  endian = niiheader$endian;

  writeBin(as.integer(niiheader$sizeof_hdr), fh, size = 4L, endian = endian);

  writeChar(niiheader$magic, fh, nchars = nchar(niiheader$magic), eos = NULL);
  writeBin(as.raw(rep(0L, (8L - nchar(niiheader$magic)))), fh, endian = endian); # fill remaining space up to max 8 bytes with zeroes.

  writeBin(as.integer(niiheader$datatype), fh, size = 2L, endian = endian);

  writeBin(as.integer(niiheader$bitpix), fh, size = 2L, endian = endian);

  writeBin(as.integer(niiheader$dim), fh, size = 8L, endian = endian);

  writeBin(as.double(niiheader$intent_p1), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$intent_p2), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$intent_p3), fh, size = 8L, endian = endian);

  writeBin(as.double(niiheader$pix_dim), fh, size = 8L, endian = endian);

  writeBin(as.integer(niiheader$vox_offset), fh, size = 8L, endian = endian);

  writeBin(as.double(niiheader$scl_slope), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$scl_inter), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$cal_max), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$cal_min), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$slice_duration), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$toffset), fh, size = 8L, endian = endian);

  writeBin(as.integer(niiheader$slice_start), fh, size = 8L, endian = endian);
  writeBin(as.integer(niiheader$slice_end), fh, size = 8L, endian = endian);

  writeChar(niiheader$descrip, fh, nchars = nchar(niiheader$descrip), eos = NULL);
  writeBin(as.raw(rep(0L, (80L - nchar(niiheader$descrip)))), fh, endian = endian); # fill remaining space up to max 80 bytes with zeroes.

  writeChar(niiheader$aux_file, fh, nchars = nchar(niiheader$aux_file), eos = NULL);
  writeBin(as.raw(rep(0L, (24L - nchar(niiheader$aux_file)))), fh, endian = endian); # fill remaining space up to max 24 bytes with zeroes.

  writeBin(as.integer(niiheader$qform_code), fh, size = 4L, endian = endian);
  writeBin(as.integer(niiheader$sform_code), fh, size = 4L, endian = endian);

  writeBin(as.double(niiheader$quatern_b), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$quatern_c), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$quatern_d), fh, size = 8L, endian = endian);

  writeBin(as.double(niiheader$qoffset_x), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$qoffset_y), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$qoffset_z), fh, size = 8L, endian = endian);

  writeBin(as.double(niiheader$srow_x), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$srow_y), fh, size = 8L, endian = endian);
  writeBin(as.double(niiheader$srow_z), fh, size = 8L, endian = endian);

  writeBin(as.integer(niiheader$slice_code), fh, size = 4L, endian = endian);
  writeBin(as.integer(niiheader$xyzt_units), fh, size = 4L, endian = endian);
  writeBin(as.integer(niiheader$intent_code), fh, size = 4L, endian = endian);

  writeChar(niiheader$intent_name, fh, nchars = nchar(niiheader$intent_name), eos = NULL);
  writeBin(as.raw(rep(0L, (16L - nchar(niiheader$intent_name)))), fh, endian = endian); # fill remaining space up to max 16 bytes with zeroes.

  writeBin(as.integer(niiheader$dim_info), fh, size = 1L, endian = endian);

  # add unused_str of length 15. Reserved for header extensions.
  writeBin(as.raw(rep(0L, 15L)), fh, endian = endian); # fill with zeroes

  # add zero padding up to 'vox_offset'.
  position_now = 540L;
  num_to_fill = as.integer(niiheader$vox_offset) - position_now;
  writeBin(as.integer(rep(0L, num_to_fill)), fh, size = 1L, endian = endian);

  # Write data.
  if(as.integer(niiheader$datatype) %in% c(2L, 4L, 8L, 512L, 768L)) {  # integer NIFTI data types
    if(! is.integer(niidata)) {
      warning("Found NIFTI integer datatype '%d' in niiheader, but niidata datatype is not integer. Converting data to integer as specified in header.\n", niiheader$datatype);
    }
    data_written = as.integer(niidata);
    writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian);
  } else { # treat as double
    if(! is.double(niidata)) {
      warning("Found NIFTI floating point datatype '%d' in niiheader, but niidata datatype is not floating point. Converting data to float as specified in header.\n", niiheader$datatype);
    }
    data_written = as.double(niidata);
    writeBin(data_written, fh, size = as.integer(niiheader$bitpix / 8L), endian = endian);
  }
  close(fh);
  return(invisible(list('header'=niiheader, 'data'=data_written)));
}


#' @keywords internal
pad.string <- function(input_string, req_length, fill_with = " ") {
  num_missing = req_length - nchar(input_string);
  if(num_missing > 0L) {
    padding = paste(replicate(num_missing, fill_with), collapse = "");
    return(paste(c(input_string, padding), collapse = ""));
  } else {
    return(input_string);
  }
}


#' @title Write given dstring to binary file, fill with zeroes to reach a total length of 'nchars'.
#' @keywords internal
write.char.zero.fill <- function(dstring, filehandle, nchars) {
  cat(sprintf("Writing string '%s'...\n", dstring));
  tryCatch({
    writeChar(as.character(dstring), filehandle, nchars = nchars, eos = NULL);
  }, warning = function(w) {
    cat(sprintf(w));
  }, error = function(e) {
    stop(sprintf("Could not write string '%s' of length %d to file handle: '%s'.\n", dstring, nchars, e));
  });
  return(invisible(NULL));
}

