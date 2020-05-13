# Functions to derive information from the MGH header.

#' @title  Compute vox2ras matrix from basic MGH header fields.
#'
#' @description This is also known as the 'scanner' or 'native' vox2ras.  It is the inverse of the respective ras2vox, see \code{\link[freesurferformats]{mghheader.ras2vox}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.vox2ras(vdh$header);
#'
#' @export
mghheader.vox2ras <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive vox2ras matrix.');
  }

  delta = c(header$internal$xsize, header$internal$ysize, header$internal$zsize);
  D = diag(delta);
  Mdc = matrix(c(header$internal$x_r, header$internal$x_a, header$internal$x_s, header$internal$y_r, header$internal$y_a, header$internal$y_s, header$internal$z_r, header$internal$z_a, header$internal$z_s), nrow=3, byrow = FALSE);
  Pcrs_c = c(header$internal$width/2, header$internal$height/2, header$internal$depth/2); # CRS of the center of the volume
  Pxyz_c = c(header$internal$c_r, header$internal$c_a, header$internal$c_s); # x,y,z at center of the volume
  Mdc_scaled = Mdc %*% D;
  Pxyz_0 = Pxyz_c - (Mdc_scaled %*% Pcrs_c); # the x,y,z location at CRS=0
  M = matrix(rep(0, 16), nrow=4);
  M[1:3,1:3] = as.matrix(Mdc_scaled);
  M[4,1:4] = c(0,0,0,1); # affine row
  M[1:3,4] = Pxyz_0;
  return(M);
}


#' @title Check whether header contains valid ras information
#'
#' @param header mgh header or `fs.volume` instance with header
#'
#' @return logical, whether header contains valid ras information (according to the `ras_good_flag`).
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.is.ras.valid(vdh$header);
#'
#' @export
mghheader.is.ras.valid <- function(header) {
  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(is.null(header)) {
    return(FALSE);
  }

  if(!is.list(header)) {
    return(FALSE); # most likely someone passed the raw volume data array instead of the volume.
  }

  if(is.null(header$ras_good_flag)) {
    return(FALSE);
  }

  if(header$ras_good_flag == 1L) {
    return(TRUE);
  }
  return(FALSE);
}


#' @title  Compute ras2vox matrix from basic MGH header fields.
#'
#' @description This is also known as the 'scanner' or 'native' ras2vox. It is the inverse of the respective vox2ras, see \code{\link[freesurferformats]{mghheader.vox2ras}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.ras2vox(vdh$header);
#'
#' @export
mghheader.ras2vox <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive ras2vox matrix.');
  }

  return(solve(mghheader.vox2ras(header)));
}




#' @title  Compute vox2ras-tkreg matrix from basic MGH header fields.
#'
#' @description This is also known as the 'tkreg' vox2ras. It is the inverse of the respective ras2vox, see \code{\link[freesurferformats]{mghheader.ras2vox.tkreg}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.vox2ras.tkreg(vdh$header);
#'
#' @export
mghheader.vox2ras.tkreg <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive vox2ras.tkreg matrix.');
  }

  header_copy = header; # copy the xsize, ysize, zsize

  # now set tkreg default orientation
  header_copy$internal$x_r = -1;
  header_copy$internal$y_r = 0;
  header_copy$internal$z_r = 0;
  header_copy$internal$c_r = 0.0;
  header_copy$internal$x_a = 0;
  header_copy$internal$y_a = 0;
  header_copy$internal$z_a = 1;
  header_copy$internal$c_a = 0.0;
  header_copy$internal$x_s = 0;
  header_copy$internal$y_s = -1;
  header_copy$internal$z_s = 0;
  header_copy$internal$c_s = 0.0;
  return(mghheader.vox2ras(header_copy));
}


#' @title  Compute ras2vox-tkreg matrix from basic MGH header fields.
#'
#' @description This is also known as the 'tkreg' ras2vox. It is the inverse of the respective vox2ras, see \code{\link[freesurferformats]{mghheader.vox2ras.tkreg}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.ras2vox.tkreg(vdh$header);
#'
#' @export
mghheader.ras2vox.tkreg <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive ras2vox.tkreg matrix.');
  }

  return(solve(mghheader.vox2ras.tkreg(header)));
}


#' @title  Compute tkreg-RAS to scanner-RAS matrix from basic MGH header fields.
#'
#' @description This is also known as the 'tkreg2scanner' matrix. Note that this is a RAS-to-RAS matrix. It is the inverse of the 'scanner2tkreg' matrix, see \code{\link[freesurferformats]{mghheader.scanner2tkreg}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.tkreg2scanner(vdh$header);
#'
#' @export
mghheader.tkreg2scanner <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive tkreg2scanner matrix.');
  }

  vox2ras_native = mghheader.vox2ras(header);
  ras2vox_tkreg = mghheader.ras2vox.tkreg(header);
  tkreg2scanner = vox2ras_native %*% ras2vox_tkreg;
  return(tkreg2scanner);
}


#' @title  Compute scanner-RAS 2 tkreg-RAS matrix from basic MGH header fields.
#'
#' @description This is also known as the 'scanner2tkreg' matrix. Note that this is a RAS-to-RAS matrix. It is the inverse of the 'tkreg2scanner' matrix, see \code{\link[freesurferformats]{mghheader.tkreg2scanner}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     mghheader.scanner2tkreg(vdh$header);
#'
#' @export
mghheader.scanner2tkreg <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    stop('MGH header does not contain valid RAS information. Cannot derive scanner2tkreg matrix.');
  }

  ras2vox = mghheader.ras2vox(header);
  vox2tkras = mghheader.vox2ras.tkreg(header);
  scanner2tkreg = vox2tkras %*% ras2vox;
  return(scanner2tkreg);
}


#' @title Compute vox2vox matrix between two volumes.
#'
#' @param header_from the MGH header of the source volume
#'
#' @param header_to the MGH header of the target volume
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @export
mghheader.vox2vox <- function(header_from, header_to) {

  if(is.fs.volume(header_from)) {
    header_from = header_from$header;
  }

  if(!(mghheader.is.ras.valid(header_from))) {
    stop("MGH header of parameter 'header_from' does not contain valid RAS information. Cannot derive vox2vox matrix.");
  }

  if(is.fs.volume(header_to)) {
    header_to = header_to$header;
  }

  if(!(mghheader.is.ras.valid(header_to))) {
    stop("MGH header of parameter 'header_to' does not contain valid RAS information. Cannot derive vox2vox matrix.");
  }

  vox2ras_from = mghheader.vox2ras(header_from);
  ras2vox_to = mghheader.ras2vox(header_to);
  vox2vox = ras2vox_to %*% vox2ras_from;
  return(vox2vox);
}


#' @title Determine whether an MGH volume is conformed.
#'
#' @description In the FreeSurfer sense, *conformed* means that the volume is in coronal primary slice direction, has dimensions 256x256x256 and a voxel size of 1 mm in all 3 directions. The slice direction can only be determined if the header contains RAS information, if it does not, the volume is not conformed.
#'
#' @param header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @return logical, whether the volume is *conformed*.
#'
#' @export
mghheader.is.conformed <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    return(FALSE);
  }

  return(mgh.is.conformed(header));
}


#' @title Compute MGH primary slice direction
#'
#' @param header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @return character string, the slice direction. One of 'sagittal', 'coronal', 'axial' or 'unknown'.
#'
#' @export
mghheader.primary.slice.direction <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    return('unknown');
  }

  mdc = header$internal$Mdc;
  return(get.slice.orientation(mdc)$direction_name);
}


#' @title Compute MGH volume orientation string.
#'
#' @param header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @return character string of length 3, one uppercase letter per axis. Each of the three position is a letter from the alphabet: `LRISAP?`. The meaning is `L` for left, `R` for right, `I` for inferior, `S` for superior, `P` for posterior, `A` for anterior. If the direction cannot be computed, all three characters are `?` for unknown. Of course, each axis (`L/R`, `I/S`, `A/P`) is only represented once in the string.
#'
#' @export
mghheader.crs.orientation <- function(header) {

  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(!(mghheader.is.ras.valid(header))) {
    return('???');
  }

  mdc = header$internal$Mdc;
  return(get.slice.orientation(mdc)$orientation_string);
}


#' @title Constructor to init MGH header instance.
#'
#' @param dims integer vector of length 4, the header dimensions. Example: \code{c(256L, 256L, 256L, 1L)}.
#'
#' @param mri_dtype_code integer, a valid MRI datatype. See \code{\link[freesurferformats]{translate.mri.dtype}}.
#'
#' @return a named list representing the header
#'
#' @keywords internal
mghheader <- function(dims, mri_dtype_code) {
  if(length(dims) != 4L) {
    stop("Parameter dims must have length 4.");
  }

  if(! is.integer(mri_dtype_code)) {
    stop("Parameter 'mri_dtype_code' must be an integer.");
  }

  dtype_name = translate.mri.dtype(mri_dtype_code); # The function is (ab)used to check the passed value, the return value is not used.

  header = list();
  header$internal = list();
  header$dtype = mri_dtype_code;
  header$nbytespervox = mri_dtype_numbytes(mri_dtype_code);
  header$ras_good_flag = 0L;
  header$internal$width = dims[1];
  header$internal$height = dims[2];
  header$internal$depth = dims[3];
  header$internal$nframes = dims[4];

  header$voldim = c(dims[1], dims[2], dims[3], dims[4]);
  header$voldim_orig = header$voldim;

  #cat(sprintf("Created MGH header with dimensions %s.\n", paste(header$voldim, collapse="x")));

  header$has_mr_params = 0L;
  header$mr = list("tr"=0.0, "te"=0.0, "ti"=0.0, "flip_angle_degrees"=0.0, "fov"=0.0);
  return(header);
}


#' @title Update mghheader fields from vox2ras matrix.
#'
#' @param header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @param vox2ras 4x4 numerical matrix, the vox2ras transformation matrix.
#'
#' @return a named list representing the header
#'
#' @export
mghheader.update.from.vox2ras <- function(header, vox2ras) {

  # see mri.cpp MRIsetVox2RASFromMatrix

  updated_header = header;

  if(! is.matrix(vox2ras)) {
    stop("Parameter 'vox2ras' must be a numerical 4x4 matrix.");
  }

  rx = vox2ras[1, 1];
  ry = vox2ras[1, 2];
  rz = vox2ras[1, 3];
  ax = vox2ras[2, 1];
  ay = vox2ras[2, 2];
  az = vox2ras[2, 3];
  sx = vox2ras[3, 1];
  sy = vox2ras[3, 2];
  sz = vox2ras[3, 3];

  # The next 3 values encode the RAS coordinate of the first voxel, i.e., the voxel at CRS=c(1,1,1) in R-indexing or (0,0,0 in C-indexing).
  P0r = vox2ras[1, 4];
  P0a = vox2ras[2, 4];
  P0s = vox2ras[3, 4];

  xsize = sqrt(rx * rx + ax * ax + sx * sx);
  ysize = sqrt(ry * ry + ay * ay + sy * sy);
  zsize = sqrt(rz * rz + az * az + sz * sz);

  if(any(abs(c(xsize, ysize, zsize) - c(updated_header$internal$xsize, updated_header$internal$ysize, updated_header$internal$zsize)) > 0.001)) {
    message(sprintf("mghheader.update.from.vox2ras: Voxel sizes inconsistent, matrix may contain shear, which is not supported."));
  }

  updated_header$internal$x_r = rx / xsize;
  updated_header$internal$x_a = ax / xsize;
  updated_header$internal$x_s = sx / xsize;

  updated_header$internal$y_r = ry / ysize;
  updated_header$internal$y_a = ay / ysize;
  updated_header$internal$y_s = sy / ysize;

  updated_header$internal$z_r = rz / zsize;
  updated_header$internal$z_a = az / zsize;
  updated_header$internal$z_s = sz / zsize;

  updated_header$internal$Mdc = matrix(c(updated_header$internal$x_r, updated_header$internal$x_a, updated_header$internal$x_s, updated_header$internal$y_r, updated_header$internal$y_a, updated_header$internal$y_s, updated_header$internal$z_r, updated_header$internal$z_a, updated_header$internal$z_s), nrow = 3, byrow = FALSE);

  # Compute and set the RAS coordinates of the center voxel, given the RAS coordinates of the first voxel.
  updated_header$ras_good_flag = 1L;
  center_voxel_ras_coords = mghheader.centervoxelRAS.from.firstvoxelRAS(updated_header, c(P0r, P0a, P0s));
  updated_header$internal$c_r = center_voxel_ras_coords[1];
  updated_header$internal$c_a = center_voxel_ras_coords[2];
  updated_header$internal$c_s = center_voxel_ras_coords[3];

  return(updated_header);
}


#' @title Compute RAS coords of center voxel.
#'
#' @param header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}. The `c_r`, `c_a` and `c_s` values in do not matter of course, they are what is computed by this function.
#'
#' @param first_voxel_RAS numerical vector of length 3, the RAS coordinate of the first voxel in the volume. The first voxel is the voxel with `CRS=1,1,1` in R, or `CRS=0,0,0` in C/FreeSurfer. This value is also known as *P0 RAS*.
#'
#' @return numerical vector of length 3, the RAS coordinate of the center voxel. Also known as *CRAS* or *center RAS*.
#'
#' @export
mghheader.centervoxelRAS.from.firstvoxelRAS <- function(header, first_voxel_RAS) {

  if(length(first_voxel_RAS) != 3L) {
    stop("Parameter 'first_voxel_RAS' must be a numerical vector of length 3.");
  }

  # Set the missing header values to arbitrary values for now, if needed.
  if(is.null(header$internal$c_r)) {
    header$internal$c_r = 0.0;
  }
  if(is.null(header$internal$c_a)) {
    header$internal$c_a = 0.0;
  }
  if(is.null(header$internal$c_s)) {
    header$internal$c_s = 0.0;
  }

  incomplete_vox2ras = mghheader.vox2ras(header);
  incomplete_vox2ras[1, 4] = first_voxel_RAS[1];
  incomplete_vox2ras[2, 4] = first_voxel_RAS[2];
  incomplete_vox2ras[3, 4] = first_voxel_RAS[3];

  center_voxel_CRS = c(header$internal$width / 2.0, header$internal$height / 2.0, header$internal$depth / 2.0, 1.0);
  center_voxel_RAS = incomplete_vox2ras %*% center_voxel_CRS;
  return(center_voxel_RAS[1:3]);
}
