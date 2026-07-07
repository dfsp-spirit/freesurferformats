#' @title Write an fs.volume instance to a file in MGH, MGZ or NIFTI v1 format.
#'
#' @description Write brain volume data to a file. The format is determined from the file extension of `filepath`.
#'
#' @param filepath string. Full path to the output file. The file extension determines the format: '.mgh' or '.mgz' for FreeSurfer MGH/MGZ format, '.nii' or '.nii.gz' for NIFTI v1 format.
#'
#' @param fs_vol an `fs.volume` instance, as returned by \code{\link{read.fs.volume}} with parameter `with_header=TRUE`.
#'
#' @note When writing NIFTI files, this function uses \code{\link{nii1header.for.mgh}} to compute a NIFTI v1 header from the MGH header information.
#'
#' @examples
#' \dontrun{
#'     mgh_file = system.file("extdata", "brain.mgz",
#'                             package = "freesurferformats",
#'                             mustWork = TRUE);
#'     fs_vol = read.fs.volume(mgh_file, with_header = TRUE);
#'     write.fs.volume(tempfile(fileext=".mgz"), fs_vol);
#'     write.fs.volume(tempfile(fileext=".nii.gz"), fs_vol);
#' }
#'
#' @family volume export functions
#'
#' @export
write.fs.volume <- function(filepath, fs_vol) {
  if(! is.fs.volume(fs_vol)) {
    stop("Parameter 'fs_vol' must be an fs.volume instance.");
  }

  if(endsWith(tolower(filepath), "mgh") | endsWith(tolower(filepath), "mgz")) {
    freesurferformats::write.fs.mgh(filepath, fs_vol$data, fs_vol$header$vox2ras_matrix);
  } else if(endsWith(tolower(filepath), "nii") | endsWith(tolower(filepath), "nii.gz")) {
    niiheader = freesurferformats::nii1header.for.mgh(fs_vol);
    freesurferformats::write.nifti1(filepath, fs_vol$data, niiheader = niiheader);
  } else {
    stop("Invalid file extension for filepath supplied to 'write.fs.volume'. Use one of 'mgh', 'mgz', 'nii', or 'nii.gz'.");
  }
}


#' @title Compute quaternion representation of a rotation from a 4x4 rotation matrix.
#'
#' @param m the input 4x4 matrix encoding the rotation, with homogeneous column [0,0,0,1].
#'
#' @return numeric vector of length 4, the quaternion representation (qw, qx, qy, qz).
#'
#' @keywords internal
m44_to_quaternion <- function(m) {
  m00 <- m[1, 1]
  m01 <- m[1, 2]
  m02 <- m[1, 3]
  m10 <- m[2, 1]
  m11 <- m[2, 2]
  m12 <- m[2, 3]
  m20 <- m[3, 1]
  m21 <- m[3, 2]
  m22 <- m[3, 3]

  tr <- m00 + m11 + m22 + 1.0

  # Algorithm from: https://github.com/NIFTI-Imaging/nifti_clib/blob/master/niftilib/nifti1_io.c
  if( tr > 0.5 ) {
    S <- sqrt( tr ) * 2.0
    qw <- 0.25 * S
    qx <- (m21 - m12) / S
    qy <- (m02 - m20) / S
    qz <- (m10 - m01) / S
  } else {
    Sx <- sqrt(1.0 + m00 - m11 - m22) * 2.0 # S = 4 * qx
    Sy <- sqrt(1.0 + m11 - m00 - m22) * 2.0 # S = 4 * qy
    Sz <- sqrt(1.0 + m22 - m00 - m11) * 2.0 # S = 4 * qz

    if( Sx > 2.0 ) {
      qw <- (m21 - m12) / Sx
      qx <- 0.25 * Sx
      qy <- (m01 + m10) / Sx
      qz <- (m02 + m20) / Sx
    } else if ( Sy > 2.0 ) {
      qw <- (m02 - m20) / Sy
      qx <- (m01 + m10) / Sy
      qy <- 0.25 * Sy
      qz <- (m12 + m21) / Sy
    } else {
      qw <- (m10 - m01) / Sz
      qx <- (m02 + m20) / Sz
      qy <- (m12 + m21) / Sz
      qz <- 0.25 * Sz
    }

    if( tr < 0.0 ) {
      qx <- -qx
      qy <- -qy
      qz <- -qz
    }

  }
  return(c(qw, qx, qy, qz))
}


#' @title Create a NIFTI v1 header from the header information contained in an fs.volume instance.
#'
#' @param mgh an `fs.volume` instance, or a string. If a string, it is interpreted as a filepath to a volume file (NIFTI, MGH or MGZ) that will be loaded with \code{\link{read.fs.volume}}.
#'
#' @param endian character string, the endianness to use. Either 'little' or 'big'. Defaults to 'little'.
#'
#' @return a NIFTI v1 header structure (see \code{\link{ni1header.template}}). Note that the header may or may not contain full RAS information, depending on whether the source `fs.volume` contained such information or not. If the MGH header does not have valid RAS information, the qform and sform codes will be set to 0 (unknown).
#'
#' @note This is intended to be used with \code{\link{write.nifti1}}, which allows users to convert MGH/MGZ data to NIFTI files.
#'
#' @family nifti1 writers
#'
#' @export
nii1header.for.mgh <- function(mgh, endian="little") {
  if (is.character(mgh)) {
    mgh = freesurferformats::read.fs.volume(mgh, with_header = TRUE);
  }
  if (! freesurferformats::is.fs.volume(mgh)) {
    stop("Parameter 'mgh' must be an fs.volume instance or a path to a file that can be loaded with 'read.fs.volume', resulting in an fs.volume instance.");
  }
  mgh_header = mgh$header;
  if(! freesurferformats::is.mghheader(mgh_header)) {
    stop("Given or loaded fs.volume instance has no valid MGH header information.");
  }

  nii_header = freesurferformats::ni1header.template();
  nii_header$endian = endian;

  # Data type mapping: MGH dtype -> NIFTI datatype/bitpix
  dtype_info = nifti.dtypebitpix.info.from.mgh.dtype(mgh_header$dtype);
  nii_header$datatype = dtype_info$datatype;
  nii_header$bitpix = dtype_info$bitpix;

  # Image dimensions
  dd = mgh_header$voldim_orig;  # c(ndim1, ndim2, ndim3, nframes)
  nii_header$dim = nifti.datadim.to.dimfield(dd);

  # Voxel dimensions: xsize, ysize, zsize, TR
  nii_header$pix_dim[2] = mgh_header$internal$xsize;
  nii_header$pix_dim[3] = mgh_header$internal$ysize;
  nii_header$pix_dim[4] = mgh_header$internal$zsize;
  if(length(dd) >= 4L & dd[4] > 1L) {
    nii_header$pix_dim[5] = mgh_header$internal$tr;
  }

  # Spatial transformation: compute sform and qform from the vox2ras matrix.
  if(mghheader.is.ras.valid(mgh_header)) {
    vox2ras = mghheader.vox2ras(mgh_header);

    # SForm: directly from vox2ras matrix (rows 1-3).
    nii_header$sform_code = 1L;
    nii_header$srow_x = vox2ras[1, ];
    nii_header$srow_y = vox2ras[2, ];
    nii_header$srow_z = vox2ras[3, ];

    # QForm: extract rotation and decompose into quaternions.
    # First, extract the 3x3 rotation part and normalize columns to remove voxel size scaling.
    rot <- vox2ras;                         # 4x4 matrix
    rot[, 4] <- c(0, 0, 0, 1);             # Ensure homogeneous column is neutral
    rot <- t(t(rot) / sqrt(colSums(rot^2)));# Normalize columns -> pure rotation matrix
    if( det(rot) < 0 ) {
      # Negative determinant: flip 3rd column, indicate via qfac (pix_dim[1]).
      rot[, 3] <- -rot[, 3];
      nii_header$pix_dim[1] <- -1.0;
    } else {
      nii_header$pix_dim[1] <- 1.0;
    }
    quatern <- m44_to_quaternion(rot);
    nii_header$quatern_b <- quatern[2];
    nii_header$quatern_c <- quatern[3];
    nii_header$quatern_d <- quatern[4];

    nii_header$qform_code <- 1L;
    nii_header$qoffset_x <- vox2ras[1, 4];
    nii_header$qoffset_y <- vox2ras[2, 4];
    nii_header$qoffset_z <- vox2ras[3, 4];
  }

  # Spatial and temporal units: NIFTI code for mm (2) + msec (8) = 10
  nii_header$xyzt_units = 10L;

  # Data range
  nii_header$cal_min = min(mgh$data, na.rm = TRUE);
  nii_header$cal_max = max(mgh$data, na.rm = TRUE);

  # Description
  nii_header$descrip = 'freesurferformats mgh2nii';

  # Standard NIFTI v1 single-file voxel offset
  nii_header$vox_offset = 352.0;

  nifti.header.check(nii_header, nifti_version = 1L);
  return(nii_header);
}
