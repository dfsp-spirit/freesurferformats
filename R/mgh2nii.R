# Functions for converting between NIFTI and fs.volume instances. (Despite the file name, the other way around is not implemented yet.)


# A note on the NIFTI coordinate system, from: 'Orientation informtion' on https://brainder.org/2012/09/23/the-nifti-file-format/
# "The world coordinate system is assumed to be ras: +x is Right, +y is Anterior and +z is Superior"


#' @title Turn an `oro.nifti` instance into an `fs.volume` instance with complete header.
#'
#' @description This is work in progress. This function takes an `oro.nifti` instance and computes the MGH header fields from the NIFTI header data, allowing for proper orientation of the contained image data (see \code{\link[freesurferformats]{mghheader.vox2ras}} and related functions). Currently only few datatypes are supported, and the `sform` header field needs to be present in the NIFTI instance.
#'
#' @param nifti_img instance of class `nifti` from the `oro.nifti` package
#'
#' @return an `fs.volume` instance. The `header` fields are computed from the NIFTI header. The `data` array is rotated into FreeSurfer storage order, but otherwise returned as present in the input NIFTI instance, i.e., no values are changed in any way.
#'
#' @seealso \code{\link[oro.nifti]{readNIfTI}}, \code{\link[freesurferformats]{read.fs.mgh}}
#'
#' @references \href{https://nifti.nimh.nih.gov/nifti-1/}{NIfTI-1 data format spec}
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


    if(!(nifti_img@magic == "n+1" | nifti_img@magic == "ni1")) {
      stop(sprintf("Unknown NIFTI magic code '%s', file format not supported. Expected magic code 'n+1' or 'ni1'.\n", nifti_img@magic));
    }

    ## --------------------- Perform some basic sanity checks on the Nifti header. ---------------------
    scale_data = FALSE; # whether data scaling is required. This is scaling based on the scaling part in the header, not to be confused with scaling due to time/space units.
    if(nifti_img@scl_slope != 0.0) {
      if(!(nifti_img@scl_slope == 1.0 & nifti_img@scl_inter == 0.0)) {
        scale_data = TRUE;
      }
    }
    if(scale_data) {
      # Most likely, data scaling is handled by oro.nifti, but I did not check it yet.
      stop(sprintf("Detected that Nifti data needs scaling (@scl_slope=%.2f, @scl_inter=%.2f), but scaling not implemented yet.\n", nifti_img@scl_slope, nifti_img@scl_inter));
    }

    # The intent code describes how to interprete the data (e.g., that the values describe a certain distribution or whatever).
    # See https://brainder.org/2012/09/23/the-nifti-file-format/ or the NIFTI spec for details.
    # There is no field for this in MGH header afaict, so we ignore it.
    if(nifti_img@intent_code != 0L) {
      warning("NIFTI intent_code '%d' ignored.\n", nifti_img@intent_code);
    }

    ## -----Check the datatype ------
    MRI_UCHAR = translate.mri.dtype("MRI_UCHAR");
    MRI_INT = translate.mri.dtype("MRI_INT");
    MRI_FLOAT = translate.mri.dtype("MRI_FLOAT");
    MRI_SHORT = translate.mri.dtype("MRI_SHORT");


    if(nifti_img@datatype == 2L & nifti_img@bitpix == 8L) {   # NIFTI: 'unsigned char'
      dtype = MRI_UCHAR;
    } else if(nifti_img@datatype == 4L & nifti_img@bitpix == 16L) {  # NIFTI: 'signed short'
      dtype = MRI_SHORT;
    } else if(nifti_img@datatype == 8L & nifti_img@bitpix == 32L) {  # NIFTI: 'signed int'
      dtype = MRI_INT;
    } else if(nifti_img@datatype == 16L & nifti_img@bitpix == 32L) {  # NIFTI: 'float'
      dtype = MRI_FLOAT;
    } else {
      stop(sprintf("Nifti images with @datatype=%d and @bitpix=%d not supported yet.\n", nifti_img@datatype, nifti_img@bitpix));
    }

    bytes_per_voxel = mri_dtype_numbytes(dtype); # Note that we store the size in **bytes** per voxel, while the Nifti header uses **bits**.

    cat(sprintf("Nifti header: Nifti datatype=%d with %d bitpix. MRI datatype '%s' (code %d), with %d bytes per voxel.\n", nifti_img@datatype, nifti_img@bitpix, translate.mri.dtype(dtype), dtype, bytes_per_voxel));

    ## ----- Check image dimensions -----
    num_used_dimensions = nifti_img@dim_[1];
    byte_swap = FALSE;
    if(num_used_dimensions < 1L | num_used_dimensions > 7L) {
      # If the first field is outside the range 1-7, it means that the data has opposite endianness and must be byte-swapped.
      # The oro.nifti library most likely handles this just fine, but I did not check that yet, so we assume its not okay for now.
      byte_swap = TRUE;
      stop(sprintf("Byte-swapped Nifti images not supported yet.\n"));
    }

    # If the image has only 3 dimensions, set frame count to 1.
    num_frames = ifelse(num_used_dimensions < 4L, 1L, nifti_img@dim_[5]); # Note difference between @dim and @dim_. The latter contains the number of "used" dimensions at the first position.


    # If the image has more than 4 dimensions, we do not support it yet.
    if(num_used_dimensions > 4L & nifti_img@dim_[6] > 1L) {
      stop("Nifti images with more than 4 used dimensions not supported yet.");
    }

    # Check for FreeSurfer hack in number of columns (negative column count, true column count stored in @glmin field).
    # Loading these non-standard NIFTI files will not work anyways with oro.nifti I guess, so this may never happen.
    if(nifti_img@dim_[2] < 0L) {
      message(sprintf("Nifti image with FreeSurfer hack detected, assuming %d columns.\n", nifti_img@glmin));
      ncols = nifti_img@glmin;
    } else {
      ncols = nifti_img@dim_[2];
    }

    ico7_num_vertices = 163842L; # Vertex count of ICO 7 meshes, like fsaverage. If the dimensions match this, the file is assumed to
    #                              contain morphometry data stored with the FreeSurfer hack.
    is_ico7 = (ncols * nifti_img@dim_[3] * nifti_img@dim_[4] == ico7_num_vertices);

    ico7_state_string = ifelse(is_ico7, "looks like", "does NOT look like");
    cat(sprintf("Nifti header: Image has %d used dimensions. It %s ico7 morphometry data.\n", nifti_img@dim_[1], ico7_state_string));

    ## Compute space and time unit factors from @xyzt_units.
    # NIFTI can store in different units, while MGH uses fixed units. Depending on the unit used in the NIFTI, we may need to
    # rescale the values to match the MGH unit. (This is not to be confused with the data scaling fields in the NIFTI header, see scl_slope and scl_inter handling above).
    space_info = nifti.space.info(nifti_img@xyzt_units);
    space_unit_factor = space_info$scaling;
    time_info = nifti.time.info(nifti_img@xyzt_units);
    time_unit_factor = time_info$scaling;
    cat(sprintf("Space data in NIFTI file is stored in unit '%s', using scaling factor %f to match FreeSurfer unit 'mm'.\n", space_info$name, space_info$scaling));
    cat(sprintf("Time data in NIFTI file is stored in unit '%s', using scaling factor %f to match FreeSurfer unit 'ms'.\n", time_info$name, time_info$scaling));


    header = mghheader(c(ncols, nifti_img@dim_[3], nifti_img@dim_[4], num_frames), dtype);
    header$internal$xsize = nifti_img@pixdim[2]; # voxel size in x direction. Still needs scaling by space_unit_factor, see below.
    header$internal$ysize = nifti_img@pixdim[3]; # voxel size in y direction. Still needs scaling by space_unit_factor, see below.
    header$internal$zsize = nifti_img@pixdim[4]; # voxel size in z direction. Still needs scaling by space_unit_factor, see below.
    header$internal$tr = nifti_img@pixdim[5];    # TR. Still needs scaling by time_unit_factor, see below.

    # Compute the vox2ras transformation. This is the crucial part.
    # See the 'Orientation information' section on https://brainder.org/2012/09/23/the-nifti-file-format/ for interpretation
    # and a good overview.
    if(nifti_img@sform_code != 0L) { # Means that the sform is present in slots srow_x, srow_y and srow_z.
      # Extract vox2ras matrix from NIFTI sform, then init the MGH header from the vox2ras matrix.
      vox2ras = rbind(nifti_img@srow_x, nifti_img@srow_y, nifti_img@srow_z, c(0, 0, 0, 1));
      header = mghheader.update.from.vox2ras(header, vox2ras);
      cat(sprintf("Computed transformation matrix into '%s' using sform header data.\n", nifti.transform.type.name(nifti_img@sform_code)));
      header$ras_good_flag = 1L;
    } else if(nifti_img@qform_code != 0L) {
      # The qform transform is based on the 3 qoffset fields, qfac, and 4 quaternions. 3 of them are in the following header fields, the 4th one needs to be
      # computed from the other 3.
      qb = nifti_img@quatern_b;
      qc = nifti_img@quatern_c;
      qd = nifti_img@quatern_d;

      # Compute qa
      qa = 1.0 - (qb*qb + qc*qc + qd*qd);
      if (qa < 1.0e-7) {
        qa = 1.0 / sqrt(qb*qb + qc*qc + qd*qd);
        qb = qa*qb;
        qc = qa*qc;
        qd = qa*qd;
        qa = 0.0;
      } else {
        qa = sqrt(qa);
      }

      # Now construct the 3x3 rotation matrix.
      rot_mat = matrix(rep(0., 9), nrow=3L);
      rot_mat[1,1] = qa*qa + qb*qb - qc*qc - qd*qd;
      rot_mat[1,2] = 2.0 * qb * qc - 2.0 * qa * qd;
      rot_mat[1,3] = 2.0 * qb * qd + 2.0 * qa * qc;
      rot_mat[2,1] = 2.0 * qb * qc + 2.0 * qa * qd;
      rot_mat[2,2] = qa*qa + qc*qc - qb*qb - qd*qd;
      rot_mat[2,3] = 2.0 * qc * qd - 2.0 * qa * qb;
      rot_mat[3,1] = 2.0 * qb * qd - 2.0 * qa * qc;
      rot_mat[3,2] = 2.0 * qc * qd + 2.0 * qa * qb;
      rot_mat[3,3] = qa*qa + qd*qd - qc*qc - qb*qb;

      # Now use voxel sizes and translation vector to compute final transform
      qfac = nifti_img@pixdim[1];
      if(!(qfac == -1 | qfac == 1)) {     # We're in the dangerous world of floating point comparison here.
        warning(sprintf("Treating non-standard qfac value '%f' as 1.0\n.", qfac));  # R is good at it, but I would rather let the user know if anything looks suspicious.
        qfac = 1;
      }
      rot_mat[1,3] = rot_mat[1,3] * qfac;
      rot_mat[2,3] = rot_mat[2,3] * qfac;
      rot_mat[3,3] = rot_mat[3,3] * qfac;

      # Fill in header fields.
      header$internal$x_r = rot_mat[1,1];
      header$internal$y_r = rot_mat[1,2];
      header$internal$z_r = rot_mat[1,3];
      header$internal$x_a = rot_mat[2,1];
      header$internal$y_a = rot_mat[2,2];
      header$internal$z_a = rot_mat[2,3];
      header$internal$x_s = rot_mat[3,1];
      header$internal$y_s = rot_mat[3,2];
      header$internal$z_s = rot_mat[3,3];

      # Compute and set center RAS
      header$internal$c_r = (header$internal$xsize * header$internal$x_r) * (header$internal$width / 2.0) +
                            (header$internal$ysize * header$internal$y_r) * (header$internal$height / 2.0) +
                            (header$internal$zsize * header$internal$z_r) * (header$internal$depth / 2.0) + nifti_img@qoffset_x;

      header$internal$c_a = (header$internal$xsize * header$internal$x_a) * (header$internal$width / 2.0) +
                            (header$internal$ysize * header$internal$y_a) * (header$internal$height / 2.0) +
                            (header$internal$zsize * header$internal$z_a) * (header$internal$depth / 2.0) + nifti_img@qoffset_y;

      header$internal$c_s = (header$internal$xsize * header$internal$x_s) * (header$internal$width / 2.0) +
                            (header$internal$ysize * header$internal$y_s) * (header$internal$height / 2.0) +
                            (header$internal$zsize * header$internal$z_s) * (header$internal$depth / 2.0) + nifti_img@qoffset_z;
      header$ras_good_flag = 1L;


      cat(sprintf("Computed transformation matrix into '%s' using qform header data.\n", nifti.transform.type.name(nifti_img@qform_code)));
    } else {
      warning("Nifti image does not contain valid sform or qform, orientation cannot be derived and is arbitrary.");
      # Fill in more or less random orientation values, scale voxel values by xsize, ysize, zsize.
      header$internal$x_r = -1.0;
      header$internal$x_a = 0.0;
      header$internal$x_s = 0.0;
      header$internal$y_r = 0.0;
      header$internal$y_a = 1.0;
      header$internal$y_s = 0.0;
      header$internal$z_r = 0.0;
      header$internal$z_a = 0.0;
      header$internal$z_s = 1.0;
      header$internal$c_r = header$internal$xsize * header$internal$width / 2.0;
      header$internal$c_a = header$internal$ysize * header$internal$height / 2.0;
      header$internal$c_s = header$internal$zsize * header$internal$depth / 2.0;
      # Indicate missing RAS info:
      header$ras_good_flag = 0L;
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
    # TODO: Check in which storage ordering the data is saved in the NIFTI image and rotate/permute the array accordingly.
    # See https://brainder.org/2012/09/23/the-nifti-file-format/ and the official NIFTI standard.

    if(length(dim(fsvol_data)) != 4) {
      # Most likely the 4th dimension of size 1 is missing, reshape it.
      dim(fsvol_data) = c(ncols, nifti_img@dim_[3], nifti_img@dim_[4], num_frames);
    }

    fsvol = list("header"=header, "data"=fsvol_data);
    class(fsvol) = 'fs.volume';
    return(fsvol);
  } else {
    stop("The 'oro.nifti' package is required to use this functionality.");
  }
}


#' @title Compute NIFTI space unit info from xyzt_units header field.
#'
#' @param xyzt_units a single character, the `xyzt_units` NIFTI header field
#'
#' @return named list with entries: `code`: the NIFTI unit code as a decimal integer, `name`: character string, the unit name, `scaling`: float, the scaling factor for the unit, relative to the FreeSurfer space unit (`mm`).
#'
#' @keywords internal
nifti.space.info <- function(xyzt_units) {
  nifti_unit_code = bitwAnd(xyzt_units, 0x07);
  nifti_unit_name = "unknown";
  scaling = 1.0;
  if(nifti_unit_code == 1L) { nifti_unit_name = "m"; scaling = 1000.0; }
  if(nifti_unit_code == 2L) { nifti_unit_name = "mm"; scaling = 1.0; }
  if(nifti_unit_code == 3L) { nifti_unit_name = "mum";  scaling = 0.001; }
  return(list("code"=nifti_unit_code, "name"=nifti_unit_name, "scaling"=scaling));
}


#' @title Get the name of the transform type from a form code.
#'
#' @description The form code is a code stored in the `sform_code` and/or `qform_code` NIFTI header fields.
#'
#' @param form_code integer, the value retrieved from the `sform_code` or the `qform_code` NIFTI header fields
#'
#' @return character string, the meaning of the code. Usually this expresses to what the data will be aligned after application of the vox2ras transformation method. (The type of transformation to perform in order to achieve this alignment depends on whether the value was retrieved from the `sform` or the `qform` field and does not matter here.)
#'
#' @keywords internal
nifti.transform.type.name <- function(form_code) {
  if(form_code == 1L) { return("scanner_anatomical"); }
  if(form_code == 2L) { return("reference"); }
  if(form_code == 3L) { return("talairach_space"); }
  if(form_code == 4L) { return("MNI152_space"); }
  return("unknown");
}


#' @title Compute NIFTI time unit info from xyzt_units header field.
#'
#' @param xyzt_units a single character, the `xyzt_units` NIFTI header field
#'
#' @return named list with entries: `code`: the NIFTI unit code as a decimal integer, `name`: character string, the unit name, `scaling`: float, the scaling factor for the unit, relative to the FreeSurfer time unit  (`ms`).
#'
#' @keywords internal
nifti.time.info <- function(xyzt_units) {
  nifti_unit_code = bitwAnd(xyzt_units, 0x38);
  nifti_unit_name = "unknown";
  scaling = 1.0;
  if(nifti_unit_code == 8L) { nifti_unit_name = "s"; scaling = 1000.0; }
  if(nifti_unit_code == 16L) { nifti_unit_name = "ms"; scaling = 1.0; }
  if(nifti_unit_code == 24L) { nifti_unit_name = "mus"; scaling = 0.001; }
  return(list("code"=nifti_unit_code, "name"=nifti_unit_name, "scaling"=scaling));
}
