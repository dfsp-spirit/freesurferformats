

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
write.fs.volume <- function(filepath, fs_vol, format="mgh") {
  is_mgh = (endsWith(tolower(filepath), "mgh") | endsWith(tolower(filepath), "mgz"));
  if (is_mgh) {
    freesurferformats::write.fs.mgh(filepath, fs_vol$data, fs_vol$vox2ras);
  } else if (endsWith(tolower(filepath), "nii")) {
    niidata = fs_vol$data; # TODO: do we need to convert here? what about units etc?
    niiheader = freesurferformats::nii1header.for.mgh(fs_vol);
    freesurferformats::write.nifti1(filepath, niidata, niiheader = niiheader);
  } else {
    stop("Invalid file extension for filepath supplied to 'write.fs.volume', use on of 'mgh', 'mgz', or 'nii'.")
  }
}


#' @title Create a NIFTI 1 header from the header information contained in an fs.volume instance.
#'
#' @param an fs.volume instance, or a string. If a string, it is interpreted as a filepath to a volume file (NIFTI, MGH or MGZ) that should be loaded.
#'
#' @return a NIFTI 1 header structure. Note that the header may or may not contain full RAS information, depending on whether the source fs.volume contained such information or not.
#'
#' @note This is intended to be used with write.nifti1, which allows users to convert MGH/MGZ data to NIFTI files.
#'
#' @export
nii1header.for.mgh <- function(mgh) {
  if (is.character(mgh)) {
    mgh = freesurferformats::read.fs.volume(mgh, with_header = TRUE);
  }
  if (! freesurferformats::is.fs.volume(mgh)) {
    stop("Parameter 'mgh' must be an fs.volume instance or a path to a file that can be loaded with 'read.fs.volume', resulting in an fs.volume instance.");
  }
  mgh_header = mgh$header;
  if(! freesurferformats::is.mghheader(mgh_header)) {
    warning("Given or loaded fs.volume instance has no header information. Returning NULL.");
    return(NULL);
  }

  endian = 'little';    # Should we expose endianness as a function parameter? It only gets relevant when writing though, so maybe not needed here.

  nii_header = freesurferformats::ni1header.template();
  nii_header$endian = endian;
  nii_header$sizeof_hdr = 348L; # TODO: this may be the default already in the template.
  nii_header$dim <- c(3L, dim(mgh$data)[1:3], 1L, 1L, 1L, 1L)
  nii_header$intent_p1 <- 0
  nii_header$intent_p2 <- 0
  nii_header$intent_p3 <- 0
  nii_header$intent_code <- 0L

  dtype_and_bp = nifti.dtypebitpix.info.from.mgh.dtype(mgh_header$dtype);
  nii_header$datatype = dtype_and_bp$datatype;
  nii_header$bitpix = dtype_and_bp$bitpix;
  nii_header$slice_start <- 0L
  nii_header$pix_dim <- c(-1, 1, 1, 1, 0, 1, 1, 1)
  # nii_header$vox_offset <- 352L
  # nii_header$scl_slope
  # nii_header$scl_inter
  # nii_header$slice_end
  # nii_header$slice_code
  nii_header$xyzt_units <- 10L
  # nii_header$cal_max
  # nii_header$cal_min
  # nii_header$slice_duration <- 0L
  # nii_header$toffset
  # nii_header$glmax
  nii_header$qform_code <- 1L

  # Convert other MGH fields here, most importantly compute sform and qform from vox2ras of MGH.

  # extract rotation matrix from vox2ras matrix
  rot <- mgh$header$vox2ras_matrix
  rot[, 4] <- c(0,0,0,1)
  rot <- t(t(rot) / sqrt(colSums(rot^2)))
  if( det(rot) < 0 ) {
    # r13 = -r13 ; r23 = -r23 ; r33 = -r33 ;
    rot[,3] <- -rot[,3]
  }
  quatern <- m44_to_quaternion(rot)
  nii_header$quatern_b <- quatern[2]
  nii_header$quatern_c <- quatern[3]
  nii_header$quatern_d <- quatern[4]

  nii_header$sform_code <- 1L
  nii_header$qoffset_x <- mat[1, 4]
  nii_header$qoffset_y <- mat[2, 4]
  nii_header$qoffset_z <- mat[3, 4]
  nii_header$srow_x <- mat[1, ]
  nii_header$srow_y <- mat[2, ]
  nii_header$srow_z <- mat[3, ]

  return(nii_header);
}


