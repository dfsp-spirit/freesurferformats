# Functions to derive information from the MGH header.

#' @title  Compute vox2ras matrix from basic MGH header fields.
#'
#' @description This is also known as the 'scanner' or 'native' vox2ras.  It is the inverse of the respective ras2vox, see \code{\link[freesurferformats]{mghheader.ras2vox}}.
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the tranformation matrix
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
#' @return logical, whether header contains valid ras information
#'
#' @keywords internal
mghheader.is.ras.valid <- function(header) {
  if(is.fs.volume(header)) {
    header = header$header;
  }

  if(is.null(header)) {
    return(FALSE);
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
#' @return 4x4 numerical matrix, the tranformation matrix
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
#' @return 4x4 numerical matrix, the tranformation matrix
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
#' @return 4x4 numerical matrix, the tranformation matrix
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
#' @return 4x4 numerical matrix, the tranformation matrix
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
#' @return 4x4 numerical matrix, the tranformation matrix
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


#' @title Compute vox2ras xform
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the tranformation matrix
#'
#' @export
mghheader.vox2ras.xform <- function(header) {
  stop("not implemented yet")
}


#' @title Compute ras2vox xform
#'
#' @param header the MGH header
#'
#' @return 4x4 numerical matrix, the tranformation matrix
#'
#' @export
mghheader.ras2vox.xform <- function(header) {
  stop("not implemented yet")
}


#' @title Compute vox2vox matrix between two volumes.
#'
#' @param header_from the MGH header of the source volume
#'
#' @param header_to the MGH header of the target volume
#'
#' @return 4x4 numerical matrix, the tranformation matrix
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

  vox2ras_xf_from = mghheader.vox2ras.xform(header_from);
  ras2vox_xf_to = mghheader.ras2vox.xform(header_to);
  vox2vox = ras2vox_xf_to %*% vox2ras_xf_from;
  return(vox2vox);
}

