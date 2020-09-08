

#' @title Read NIFTI v1 header from file with FreeSurfer hack.
#'
#' @param filepath path to a NIFTI v1 file (single file version), which can contain the FreeSurfer hack.
#'
#' @param little_endian internal logical, leave this alone. Endianness will be figured out automatically, messing with this parameter only hurts.
#'
#' @return named list with NIFTI 1 header fields.
#'
#' @note The FreeSurfer hack is a non-standard way to save long vectors in NIFTI v1 files.
#'
#' @export
nifti.header.fshack <- function(filepath, little_endian = TRUE) {

  endian = ifelse(little_endian, "little", 'big');
  niiheader = list();

  if (endsWith(filepath, '.gz')) {
    fh = gzfile(filepath, "rb");
  }
  else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  niiheader$sizeof_hdr = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  if(niiheader$sizeof_hdr != 348L) {
    if(little_endian == FALSE) {
      stop(sprintf("File not in NIFTI 1 format: invalid header size %d, expected 348.\n", niiheader$sizeof_hdr)); # nocov
    } else {
      return(nifti.header.fshack, filepath, little_endian = FALSE);
    }
  }

  num_skip = 36L; # bytes to skip, this part is not used. It exists for compatibility with ANALYZE format.
  discarded = readBin(fh, integer(), n = num_skip, size = 1L);
  discarded = NULL;

  niiheader$dim = readBin(fh, integer(), n = 8, size = 2, endian = endian);
  niiheader$dim_raw = niiheader$dim;
  niiheader$uses_freesurfer_hack = ifelse(niiheader$dim[2] == -1L, TRUE, FALSE);

  niiheader$intent_p1 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$intent_p2 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
  niiheader$intent_p3 = readBin(fh, numeric(), n = 1, size = 4, endian = endian);

  niiheader$intent_code = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$datatype = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$bitpix = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$slice_start = readBin(fh, integer(), n = 1, size = 2, endian = endian);

  niiheader$pix_dim = readBin(fh, numeric(), n = 8, size = 4, endian = endian);
  niiheader$vox_offset = readBin(fh, numeric(), n = 1, size = 4, endian = endian);
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

  if(niiheader$uses_freesurfer_hack) {
    if(niiheader$dim[4] > 0L) {
      niiheader$dim = c(niiheader$glmin, 1L, 1L, niiheader$dim[4]);
    } else {
      niiheader$dim = c(niiheader$glmin, 1L, 1L);
    }
  }

  return(niiheader);
}
