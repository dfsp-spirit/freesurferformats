#' @title Read volume file in MGH, MGZ or NIFTI format
#'
#' @description Read multi-dimensional brain imaging data from a file.
#'
#' @param filepath string. Full path to the input MGZ, MGH or NIFTI file.
#'
#' @param format character string, one one of 'auto', 'nii', 'mgh' or 'mgz'. The format to assume. If set to 'auto' (the default), the format will be derived from the file extension.
#'
#' @inheritParams read.fs.mgh
#'
#' @return data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header. If the parameter flatten is `TRUE`, a numeric vector is returned instead. Note: The return value changes if the parameter with_header is `TRUE`, see parameter description.
#'
#' @family morphometry functions
#'
#' @seealso To derive more information from the header, see the `mghheader.*` functions, like \code{\link[freesurferformats]{mghheader.vox2ras.tkreg}}.
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vd = read.fs.volume(brain_image);
#'     cat(sprintf("Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
#'                  paste(dim(vd), collapse = ' '), min(vd), mean(vd), max(vd)));
#'     # Read it again with full header data:
#'     vdh = read.fs.volume(brain_image, with_header = TRUE);
#'     # Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
#'     vox2ras_matrix = mghheader.vox2ras(vdh)
#'     vox2ras_matrix %*% c(0,0,0,1);
#'
#' @export
read.fs.volume <- function(filepath, format = "auto", flatten = FALSE, with_header=FALSE, drop_empty_dims=FALSE) {
  format = tolower(format);
  if(!(format %in% c('auto', 'nii', 'mgh', 'mgz'))) {
    stop("Format must be one of c('auto', 'nii', 'mgh', 'mgz').");
  }

  if(format == 'nii' | (format == 'auto' & filepath.ends.with(filepath, c('.nii', '.nii.gz')))) {
    return(read.fs.volume.nii(filepath, flatten = flatten, with_header = with_header, drop_empty_dims = drop_empty_dims));
  }

  if(format == 'mgh' | format == 'mgz' | (format == 'auto' & filepath.ends.with(filepath, c('.mgh', '.mgz')))) {
    return(read.fs.mgh(filepath, flatten = flatten, with_header = with_header, drop_empty_dims = drop_empty_dims));
  }

}
