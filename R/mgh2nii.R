# Functions for converting between Nifti and fs.volume instances.


#' @title Turn an oro.nifti instance into an fs.volume instance.
#'
#' @description This is work in progress. Currently only few datatypes are supported, and the sform needs to be present in the nifti instance.
#'
#' @param nifti_img instance of class `nifti` from the `oro.nifti` package
#'
#' @return an `fs.volume` instance
#'
#' @examples
#' \dontrun{
#'    nii_file = "~/data/tim_only/tim/mri/brain.nii";  # mri_convert brain.mgz brain.nii
#'    nii_img = oro.nifti::readNIfTI(nii_file);
#' }
#'
#'@export
fs.volume.from.oro.nifti <- function(nifti_img) {
  cat(sprintf("fs.volume.from.oro.nifti: This is an experimental WIP function. Do NOT use this in production.\n"));
  if(! oro.nifti::is.nifti(nifti_img)) {
    stop("Parameter 'nifti_img' is not a nifti instance from oro.nifti.");
  }
  # slotNames(nifti_img);
  # nifti_img@pixdim;
  # mriio.cpp 9400

  ## --------------------- Perform some basic snity checks on the Nifti header. ---------------------
  if(nifti_img@scl_slope != 0.0) {
    stop(sprintf("Detected that Nifti data needs scaling (@scl_slope=%.2f), but scaling not implemented yet.\n", nifti_img@scl_slope));
  }

  ## -----Check the datatype ------
  MRI_UCHAR = translate.mri.dtype("MRI_UCHAR");
  MRI_INT = translate.mri.dtype("MRI_INT");
  MRI_FLOAT = translate.mri.dtype("MRI_FLOAT");
  MRI_SHORT = translate.mri.dtype("MRI_SHORT");


  if(nifti_img@datatype == 2L & nifti_img@bitpix == 8L) {
    dtype = MRI_UCHAR;
  } else {
    stop(sprintf("Nifti images with @datatype=%d and @bitpix=%d not supported yet.\n", nifti_img@datatype, nifti_img@bitpix));
  }

  bytes_per_voxel = mri_dtype_numbytes(dtype); # Note that we store **bytes** per voxel, while the Nifti header uses **bits**.

  cat(sprintf("Nifti header: Nifti datatype=%d with %d bitpix. MRI datatype '%s' (code %d), with %d bytes per voxel.\n", nifti_img@datatype, nifti_img@bitpix, translate.mri.dtype(dtype), dtype, bytes_per_voxel));

  ## ----- Check image dimensions -----
  # If the image has only 3 dimensions, set slice count to 1.
  num_slices = ifelse(nifti_img@dim_[1] < 4L, 1L, nifti_img@dim_[5]); # Note difference between @dim and @dim_. The latter contains the number of "used" dimensions at the first position.

  # If the image has more than 4 dimensions, we do not support it yet.
  if(nifti_img@dim_[1] > 4L) {
    stop("Nifti images with more than 4 used dimensions not supported yet.");
  }

  # Check for FreeSurfer hack in number of columns (negative column count, true column count stored in @glmin field)
  if(nifti_img@dim_[2] < 0L) {
    stop("Nifti images with FreeSurfer detected.");
    ncols = nifti_img@glmin;
  } else {
    ncols = nifti_img@dim_[2];
  }

  ico7_num_vertices = 163842L; # Vertex count of ICO 7 meshes, like fsaverage. If the dimensions match this, the file is assumed to
  #                              contain morphometry data stored with the FreeSurfer hack.
  is_ico7 = (ncols * nifti_img@dim_[3] * nifti_img@dim_[4] == ico7_num_vertices);

  ico7_state_string = ifelse(is_ico7, "looks like", "does not look like");
  cat(sprintf("Nifti header: Image has %d used dimensions. It %s ico7 morphometry data.\n", nifti_img@dim_[1], ico7_state_string));

  header = mghheader(c(ncols, nifti_img@dim_[3], nifti_img@dim_[4], nifti_img@dim_[5]), dtype);
  header$internal$xsize = nifti_img@pixdim[2]; # voxel size in x direction
  header$internal$ysize = nifti_img@pixdim[3]; # voxel size in y direction
  header$internal$zsize = nifti_img@pixdim[4]; # voxel size in z direction
  header$internal$tr = nifti_img@pixdim[5];    # TR

  if(nifti_img@sform_code == 1L) { # Means that the sform is present.
    vox2ras = rbind(nifti_img@srow_x, nifti_img@srow_y, nifti_img@srow_z, c(0, 0, 0, 1));
    header = mghheader.update.from.vox2ras(header, vox2ras);
    header$ras_good_flag = 1L;
  } else {
    stop("Nifti images without valid sform not supported yet.");
  }

}
