# Functions for converting between Nifti and fs.volume instances.


#' @title Turn an `oro.nifti` instance into an `fs.volume` instance.
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
#'    fsvolume = fs.volume.from.oro.nifti(nii_img);
#' }
#'
#'@export
fs.volume.from.oro.nifti <- function(nifti_img) {

  if (requireNamespace("oro.nifti", quietly = TRUE)) {
    cat(sprintf("fs.volume.from.oro.nifti: This is an experimental WIP function. Do NOT use this in production.\n"));
    if(! oro.nifti::is.nifti(nifti_img)) {
      stop("Parameter 'nifti_img' is not a nifti instance from oro.nifti.");
    }
    # slotNames(nifti_img);
    # nifti_img@pixdim;
    # mriio.cpp 9400

    ## --------------------- Perform some basic sanity checks on the Nifti header. ---------------------
    scale_data = FALSE; # whether data scaling is required.
    if(nifti_img@scl_slope != 0.0) {
      if(!(nifti_img@scl_slope == 1.0 & nifti_img@.scl_inter == 0.0)) {
        scale_data = TRUE;
      }
    }
    if(scale_data) {
      stop(sprintf("Detected that Nifti data needs scaling (@scl_slope=%.2f, @scl_inter=%.2f), but scaling not implemented yet.\n", nifti_img@scl_slope, nifti_img@scl_inter));
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
    num_used_dimensions = nifti_img@dim_[1];
    if(num_used_dimensions < 1L) {
      stop("Nifti images without any used dimension not supported. Check image file.");
    }

    # If the image has only 3 dimensions, set slice count to 1.
    num_frames = ifelse(num_used_dimensions < 4L, 1L, nifti_img@dim_[5]); # Note difference between @dim and @dim_. The latter contains the number of "used" dimensions at the first position.

    # If the image has more than 4 dimensions, we do not support it yet.
    if(num_used_dimensions > 4L) {
      stop("Nifti images with more than 4 used dimensions not supported yet.");
    }

    # Check for FreeSurfer hack in number of columns (negative column count, true column count stored in @glmin field).
    # This requires special handling later, which is not supported yet.
    if(nifti_img@dim_[2] < 0L) {
      stop("Nifti image with FreeSurfer hack detected.");
      ncols = nifti_img@glmin;
    } else {
      ncols = nifti_img@dim_[2];
    }

    ico7_num_vertices = 163842L; # Vertex count of ICO 7 meshes, like fsaverage. If the dimensions match this, the file is assumed to
    #                              contain morphometry data stored with the FreeSurfer hack.
    is_ico7 = (ncols * nifti_img@dim_[3] * nifti_img@dim_[4] == ico7_num_vertices);

    ico7_state_string = ifelse(is_ico7, "looks like", "does NOT look like");
    cat(sprintf("Nifti header: Image has %d used dimensions. It %s ico7 morphometry data.\n", nifti_img@dim_[1], ico7_state_string));

    ## TODO: Compute space and time unit factors from @xyzt_units
    warning("Using fixed space_unit_factor of 1.0 and time_unit_factor of 1.0. These values are wrong: computation from header not implemented yet.");
    space_unit_factor = 1.0; # TODO: adapt to unit from header: meters vs. mm vs. mum
    time_unit_factor = 1.0;  # TODO: adapt to unit from header:sec vs. msec vs. usec

    header = mghheader(c(ncols, nifti_img@dim_[3], nifti_img@dim_[4], nifti_img@dim_[5]), dtype);
    header$internal$xsize = nifti_img@pixdim[2]; # voxel size in x direction. Still needs scaling by space_unit_factor, see below.
    header$internal$ysize = nifti_img@pixdim[3]; # voxel size in y direction. Still needs scaling by space_unit_factor, see below.
    header$internal$zsize = nifti_img@pixdim[4]; # voxel size in z direction. Still needs scaling by space_unit_factor, see below.
    header$internal$tr = nifti_img@pixdim[5];    # TR. Still needs scaling by time__unit_factor, see below.

    if(nifti_img@sform_code != 0L) { # Means that the sform is present.
      # Extract vox2ras matrix from NIFTI sform, then init the MGH header from the vox2ras matrix.
      vox2ras = rbind(nifti_img@srow_x, nifti_img@srow_y, nifti_img@srow_z, c(0, 0, 0, 1));
      header = mghheader.update.from.vox2ras(header, vox2ras);
      header$ras_good_flag = 1L;
    } else if(nifti_img@qform_code != 0L) {
      stop("Nifti images without valid sform not supported yet. (Your image has a qform, but using it is not implemented yet.)");
    } else {
      stop("Nifti images without valid sform or qform not supported, orientation cannot be derived. Just extract the @.Data manually.");
    }

    header$internal$xsize = header$internal$xsize * space_unit_factor;
    header$internal$ysize = header$internal$ysize * space_unit_factor;
    header$internal$zsize = header$internal$zsize * space_unit_factor;
    header$internal$c_r = header$internal$c_r * space_unit_factor;
    header$internal$c_a = header$internal$c_a * space_unit_factor;
    header$internal$c_s = header$internal$c_s * space_unit_factor;
    header$internal$tr = header$internal$tr * time_unit_factor;

    header$has_mr_params = 1L;
    header$mr = list();
    header$mr$tr = header$internal$tr;

    # TODO: Refactor: The next block is duplicated from read.fs.mgh and should go into a separate function.
    x_half_length = header$internal$width / 2.0 * header$internal$xsize;
    y_half_length = header$internal$height / 2.0 * header$internal$ysize;
    z_half_length = header$internal$depth / 2.0 * header$internal$zsize;
    header$internal$xstart = - x_half_length;
    header$internal$xend = x_half_length;
    header$internal$ystart = - y_half_length;
    header$internal$yend = y_half_length;
    header$internal$zstart = - z_half_length;
    header$internal$zend = z_half_length;
    xfov = header$internal$xend - header$internal$xstart;
    yfov = header$internal$yend - header$internal$ystart;
    zfov = header$internal$zend - header$internal$zstart;
    header$internal$fov = ifelse(xfov > yfov, ifelse(xfov > zfov, xfov, zfov), ifelse(yfov > zfov, yfov, zfov));
    header$mr$fov = header$internal$fov;
    orientation_info = get.slice.orientation(header$internal$Mdc);
    header$internal$slice_orientation_string = orientation_info$orientation_string;
    header$internal$slice_direction_name = orientation_info$direction_name;

    fsvol_data = nifti_img@.Data;
    # TODO: Check in which ordering the data is saved in the NIFTI image and rotate/permute the array accordingly.
    # See https://brainder.org/2012/09/23/the-nifti-file-format/ and the official NIFTI standard.

    if(length(dim(fsvol_data)) != 4) {
      # Most likely the 4th dimension of size 1 is missing, reshape it.
      dim(fsvol_data) = c(ncols, nifti_img@dim_[3], nifti_img@dim_[4], nifti_img@dim_[5]);
    }

    fsvol = list("header"=header, "data"=fsvol_data);
    class(fsvol) = 'fs.volume';
    return(fsvol);
  } else {
    stop("The 'oro.nifti' package is required to use this functionality.");
  }
}
