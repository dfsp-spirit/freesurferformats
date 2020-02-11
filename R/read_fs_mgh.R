#' @title Read file in FreeSurfer MGH or MGZ format
#'
#' @description Read multi-dimensional brain imaging data from a file in FreeSurfer binary MGH or MGZ format. The MGZ format is just a gzipped version of the MGH format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/mri/T1.mgz', which contains a 3D brain scan of bert.
#'
#' @param filepath string. Full path to the input MGZ or MGH file.
#'
#' @param is_gzipped a logical value (TRUE or FALSE) or the string 'AUTO'. Whether to treat the input file as gzipped, i.e., MGZ instead of MGH format. Defaults to 'AUTO', which tries to determine this from the last three characters of the 'filepath' parameter. Files with extensions 'mgz' and '.gz' (in arbitrary case) are treated as MGZ format, all other files are treated as MGH. In the special case that 'filepath' has less than three characters, MGH is assumed.
#'
#' @param flatten logical. Whether to flatten the return volume to a 1D vector. Useful if you know that this file contains 1D morphometry data.
#'
#' @param with_header logical. Whether to return the header as well. If TRUE, return an instance of class `fs.volume` for data with at least 3 dimensions, a named list with entries "data" and "header". The latter is another named list which contains the header data. These header entries exist: "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions. E.g., c(256, 256, 256, 1). These header entries may exist: "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists if "has_mr_params" is 1). See the `mghheader.*` functions, like \code{\link[freesurferformats]{mghheader.vox2ras.tkr}}, to compute more information from the header fields.
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions of the returned data
#'
#' @return data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header. If the parameter flatten is `TRUE`, a numeric vector is returned instead. Note: The return value changes if the parameter with_header is `TRUE`, see parameter description.
#'
#' @family morphometry functions
#'
#' @seealso To derive more information from the header, see the `mghheader.*` functions, like \code{\link[freesurferformats]{mghheader.vox2ras.tkr}}.
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vd = read.fs.mgh(brain_image);
#'     cat(sprintf("Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
#'                  paste(dim(vd), collapse = ' '), min(vd), mean(vd), max(vd)));
#'     # Read it again with full header data:
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     # Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
#'     vdh$header$vox2ras_matrix %*% c(0,0,0,1);
#'
#' @export
read.fs.mgh <- function(filepath, is_gzipped = "AUTO", flatten = FALSE, with_header=FALSE, drop_empty_dims=FALSE) {


    header = list();

    if(typeof(is_gzipped) == "logical") {
        is_gz = is_gzipped;
    } else if (typeof(is_gzipped) == "character") {
        if(is_gzipped == "AUTO") {
            is_gz = guess.filename.is.gzipped(filepath);
        } else {
            stop("Argument 'is_gzipped' must be 'AUTO' if it is a string.\n");
        }
    } else {
        stop(sprintf("ERROR: Argument is_gzipped must be logical (TRUE or FALSE) or 'AUTO'.\n"));
    }

    if (is_gz) {
        fh = gzfile(filepath, "rb");
    }
    else {
        fh = file(filepath, "rb");
    }

    v = readBin(fh, integer(), n = 1, endian = "big");  # mgh version
    if(v!=1L) {
        stop("File not in MGH/MGZ format.");
    }
    ndim1 = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    ndim2 = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    ndim3  = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    nframes = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    dtype = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    dof = readBin(fh, integer(), n = 1, size = 4, endian = "big");

    header$dtype = dtype;
    header$dof = dof;
    header$internal = list();

    unused_header_space_size_left = 256L;

    ras_flag_size = 2L;
    header$ras_good_flag = readBin(fh, integer(), size = ras_flag_size, n = 1, endian = "big");
    unused_header_space_size_left = unused_header_space_size_left - ras_flag_size;
    if(header$ras_good_flag == 1L) {
        delta = readBin(fh, numeric(), n = 3, size = 4, endian = "big");  # xsize, ysize, zsize (voxel size along dimensions)
        header$internal$xsize = delta[1];   # voxel size in mm
        header$internal$ysize = delta[2];
        header$internal$zsize = delta[3];

        Mdc = readBin(fh, numeric(), n = 9, size = 4, endian = "big");    # vector of length 9:  x_r, x_a, x_s, y_r, y_a, y_s, z_r, z_a, z_s. Note: gets turned into 3x3 matrix below
        header$internal$x_r = Mdc[1];
        header$internal$x_a = Mdc[2];
        header$internal$x_s = Mdc[3];
        header$internal$y_r = Mdc[4];
        header$internal$y_a = Mdc[5];
        header$internal$y_s = Mdc[6];
        header$internal$z_r = Mdc[7];
        header$internal$z_a = Mdc[8];
        header$internal$z_s = Mdc[9];

        Mdc = matrix(Mdc, nrow=3, byrow = FALSE); # turn Mdc into 3x3 matrix

        Pxyz_c = readBin(fh, numeric(), n = 3, size = 4, endian = "big"); # 1x3 vector:  c_r, c_a, c_s
        header$internal$c_r = Pxyz_c[1];
        header$internal$c_a = Pxyz_c[2];
        header$internal$c_s = Pxyz_c[3];

        D = diag(delta);
        Pcrs_c = c(ndim1/2, ndim2/2, ndim3/2); # CRS of the center voxel
        Mdc_scaled = Mdc %*% D;
        Pxyz_0 = Pxyz_c - (Mdc_scaled %*% Pcrs_c); # the x,y,z location at CRS=0

        M = matrix(rep(0, 16), nrow=4);
        M[1:3,1:3] = as.matrix(Mdc_scaled);
        M[4,1:4] = c(0,0,0,1); # affine row
        M[1:3,4] = Pxyz_0;

        #ras_xform = matrix(rep(0, 16), nrow=4);
        #ras_xform[1:3,1:3] = Mdc;
        #ras_xform[4,1:4] = c(0,0,0,1);
        #ras_xform[1:3,4] = Pxyz_c;
        #header$ras_xform = ras_xform;

        header$internal$delta = delta;
        header$internal$Pxyz_c = Pxyz_c;
        header$internal$D = D;
        header$internal$Pcrs_c = Pcrs_c;
        header$internal$Pxyz_0 = Pxyz_0;
        header$internal$M = M;
        header$internal$Mdc = Mdc;

        header$internal$width = ndim1;   # size in number of voxels in that dimensions (256 for conformed volumes)
        header$internal$height = ndim2;
        header$internal$depth = ndim3;

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


        orientation_info = get.slice.orientation(Mdc);
        header$internal$slice_orientation_string = orientation_info$orientation_string;
        header$internal$slice_direction_name = orientation_info$direction_name;

        header$internal$fov = ifelse(xfov > yfov, ifelse(xfov > zfov, xfov, zfov), ifelse(yfov > zfov, yfov, zfov));


        header$vox2ras_matrix = as.matrix(M);

        header$internal$is_conformed = as.integer(mgh.is.conformed(header));

        RAS_space_size = (3*4 + 4*3*4);    # 60 bytes
        unused_header_space_size_left = unused_header_space_size_left - RAS_space_size;
    } else {
      header$internal$slice_orientation_string = '???';
      header$internal$slice_direction_name = 'unknown';
      header$internal$is_conformed = 0L;
    }

    # Skip to end of header/beginning of data
    if(is_gz) {   # Cannot seek in a gzip stream
      discarded = readBin(fh, integer(), n = unused_header_space_size_left, size = 1L, endian = "big");
    } else {
      seek(fh, where = unused_header_space_size_left, origin = "current");
    }

    nv = ndim1 * ndim2 * ndim3 * nframes;   # number of voxels
    volsz = c(ndim1, ndim2, ndim3, nframes);
    header$voldim_orig = volsz;
    header$voldim = volsz;   # May change later due to drop or flatten parameters

    # Determine size of voxel data, depending on dtype from header above
    MRI_UCHAR = 0L;
    MRI_INT = 1L;
    MRI_FLOAT = 3L;
    MRI_SHORT = 4L;

    dt_explanation = "0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT";
    dtype_name = translate.mri.dtype(dtype); # Validate that the dtype. Will stop if not.

    # Determine number of bytes per voxel
    nbytespervox = mri_dtype_numbytes(dtype);
    #cat(sprintf("Reading dtype %d '%s' with %d bytes per value.\n", dtype, dtype_name, nbytespervox));
    if(dtype == MRI_FLOAT) {
        data = readBin(fh, numeric(), size = nbytespervox, n = nv, endian = "big");
    } else if(dtype == MRI_UCHAR) {
        data = readBin(fh, integer(), size = nbytespervox, n = nv, signed = FALSE, endian = "big");
    } else {
        # Fine for both MRI_INT and MRI_SHORT
        data = readBin(fh, integer(), size = nbytespervox, n = nv, endian = "big");
    }
    header$nbytespervox = nbytespervox;


    num_read = prod(length(data));
    if (num_read != nv) {
        stop(sprintf(" ERROR: read %d voxel values but expected to read %d.\n", num_read, nv));
    }

    # Reshape to expected dimensions
    data = array(data, dim = c(ndim1, ndim2, ndim3, nframes));

    if(flatten) {
        dim(data) = c(nv);
        data = as.vector(unlist(data));
        header$voldim = c(length(data));
    }

    header$has_mr_params = 0;
    if(with_header) {
        # Read the mr_params footer behind the data. The mr_params footer is optional, so we do not care if reading it fails.
        ignored = tryCatch({
            header$mr_params  = readBin(fh, numeric(), n = 5, size = 4, endian = "big");
            header$has_mr_params = 1;
            header$mr = list();
            header$mr$tr = header$mr_params[1];           # repetition time [ms]
            header$mr$flip_angle_radians = header$mr_params[2];   # flip angle [radians]
            header$mr$flip_angle_degrees = header$mr_params[2] * (180./pi);   # flip angle [degrees]
            header$mr$te = header$mr_params[3];           # echo time [ms]
            header$mr$ti = header$mr_params[4];           # inversion time [ms]
            header$mr$fov = header$mr_params[5];          # field-of-view
        }, error=function(e){}, warning=function(w){});
    }

    close(fh);

    if(drop_empty_dims) {
      data = drop(data);
    }

    if(with_header) {
        return_list = list();
        return_list$header = header;
        return_list$data = data;
        class(return_list) = 'fs.volume';
        return(return_list);
    }
    return(data);
}


#' @title Guess whether a file is gzipped.
#'
#' @description Guess whether a file is gzipped, based on the file extension.
#'
#' @param filepath string. Path to a file.
#'
#' @param gz_extensions list of strings. A list of suffixes that is considered indicative for the file being gzipped. Defaults to c(".gz", ".mgz"). Case does not matter.
#'
#' @return logical, whether this function thinks the file is gzipped.
#'
#' @keywords internal
guess.filename.is.gzipped <- function(filepath, gz_extensions=c(".gz", ".mgz")) {
    return(filepath.ends.with(filepath, extensions=gz_extensions));
}



#' @title Compute MGH orientation string and direction
#'
#' @param Mdc numeric 3x3 matrix, typically from the \code{header$internal$Mdc} field as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @return named list with entries: `orientation_string`: character string of length 3, one uppercase letter per axis. `direction_name`: slice direction, character string, one of 'sagittal', 'coronal', 'axial' or 'unknown'.
#'
#' @keywords internal
get.slice.orientation <- function(Mdc) {


  if(is.null(Mdc)) {
    return(list('orientation_string'='???', 'direction_name'='unknown'));
  }

  orientation = c("?", "?", "?");
  for (char_idx in seq.int(3)) {
    sagittal = Mdc[1, char_idx];   # LR axis
    coronal = Mdc[2, char_idx];    # PA axis
    axial = Mdc[3, char_idx];      # IS axis

    if ((abs(sagittal) > abs(coronal)) & (abs(sagittal) > abs(axial))) {
      orientation[char_idx] = ifelse(sagittal > 0, 'R', 'L');
      next;
    }
    if (abs(coronal) > abs(axial)) {
      orientation[char_idx] = ifelse(coronal > 0, 'A', 'P');
      next;
    }
    orientation[char_idx] = ifelse(axial > 0, 'S', 'I');
  }
  orientation_string = paste(orientation, collapse="");

  direction_name = 'unknown';
  if(orientation[3] %in% c('R', 'L')) { direction_name = 'sagittal'; }
  if(orientation[3] %in% c('P', 'A')) { direction_name = 'coronal'; }
  if(orientation[3] %in% c('I', 'S')) { direction_name = 'axial'; }

  return(list('orientation_string'=orientation_string, 'direction_name'=direction_name));
}


#' @title Determine whether an MGH volume is conformed.
#'
#' @description In the FreeSurfer sense, *conformed* means that the volume is in coronal primary slice direction, has dimensions 256x256x256 and a voxel size of 1 mm in all 3 directions. The slice direction can only be determined if the header contains RAS information, if it does not, the volume is not conformed.
#'
#' @param mgh_header Header of the mgh datastructure, as returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @param voxel_size_tolerance double, the tolerance to accept when comparing the voxel size to the required value of `1.0`. Defaults to `1e-4`. Leave this alone unless you know what you are doing.
#'
#' @return logical, whether the volume is *conformed*.
#'
#' @keywords internal
mgh.is.conformed <- function(mgh_header, voxel_size_tolerance=1e-4) {
  if(is.null(mgh_header)) {
    stop("Parameter 'mgh_header' must not be NULL.");
  }
  if(is.null(mgh_header$ras_good_flag) | mgh_header$ras_good_flag < 1L) {
    return(FALSE);
  }
  if(mgh_header$internal$slice_direction_name != "coronal") {
    return(FALSE);
  }

  required_voxel_size = 1.0;
  if(mgh_header$internal$width == 256L & mgh_header$internal$height == 256L & mgh_header$internal$depth == 256L) {
    if(abs(required_voxel_size - mgh_header$internal$xsize) <= voxel_size_tolerance & abs(required_voxel_size - mgh_header$internal$ysize) <= voxel_size_tolerance & abs(required_voxel_size - mgh_header$internal$zsize) <= voxel_size_tolerance) {
      return(TRUE);
    }
  }
  return(FALSE);
}


#' @title Translate between code and name of MRI data types.
#'
#' @param dtype character string (one of c('MRI_FLOAT') or integer, one of c(0L, 1L, 3L, 4L). Numeric values will be converted to integer.
#'
#' @return if `dtype` is a character string, the respective integer code. If it is numeric, the respective character string.
#'
#' @keywords internal
translate.mri.dtype <- function(dtype) {
  s2i = list("MRI_UCHAR"=0L, "MRI_INT"=1L, "MRI_FLOAT"=3L, "MRI_SHORT"=4L);
  if(is.numeric(dtype)) {
    dtype = as.integer(dtype);
    if(dtype == 0L) {
      return("MRI_UCHAR");
    } else if(dtype == 1L) {
      return("MRI_INT");
    } else if(dtype == 3L) {
      return("MRI_FLOAT");
    } else if(dtype == 4L) {
      return("MRI_SHORT");
    } else {
      stop(sprintf("Invalid MRI data type code '%d'.\n", dtype));
    }
  } else {
    if(dtype %in% names(s2i)) {
      return(s2i[[dtype]]);
    } else {
      stop(sprintf("Invalid MRI data type string '%s'.\n", dtype));
    }
  }
}

#' @title Check whether object is an fs.volume
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain volume (that is, has "fs.volume" amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.volume <- function(x) inherits(x, "fs.volume")


#' @title Print description of a brain volume.
#'
#' @param x brain volume with class `fs.volume`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.volume <- function(x, ...) {
  cat(sprintf("Brain volume with %d dimensions '%s' and %d voxels.\n", length(dim(x$data)), paste(dim(x$data), collapse="x"), prod(dim(x$data))));
  cat(sprintf(" - Data dimensions according to header: '%s'.\n", paste(x$header$voldim_orig, collapse="x")));
  if(x$header$voldim_orig[2] == 1L) {
    if(x$header$voldim_orig[1] == 163842L) {
      cat(sprintf(" - Note: This looks like standard space morphometry data, length of first dimension matches fsaverage surface vertex count (163842).\n"));
    } else {
      cat(sprintf(" - Note: This looks like morphometry data (2nd dimension is 1, so this is not a volume).\n"));
    }
  }
  cat(sprintf(" - Datatype according to header is %d ('%s'), values are in range [%.2f, %.2f].\n", x$header$dtype, translate.mri.dtype(x$header$dtype), min(x$data), max(x$data)));

  # vox2ras
  if(x$header$ras_good_flag == 1) {
    cat(sprintf(" - Header contains vox2ras transformation information. Voxel size is %.2f x %.2f x %.2f mm.\n", x$header$internal$xsize, x$header$internal$ysize, x$header$internal$zsize));
    info_conformed = ifelse(x$header$internal$is_conformed == 1L, "is conformed", "is not conformed");
    cat(sprintf(" - Volume %s, pimary slice direction is '%s', orientation is '%s'.\n", info_conformed, x$header$internal$slice_direction_name, x$header$internal$slice_orientation_string));
  } else {
    cat(sprintf(" - Header does not contain vox2ras transformation information.\n"));
  }

  # mr_params
  if(x$header$has_mr_params == 1) {
    cat(sprintf(" - Header contains MR acquisition parameters: TR: %.2f msec, TE: %.2f msec, TI: %.2f msec, flip angle: %.2f degrees, fov = %.3f.\n", x$header$mr$tr, x$header$mr$te, x$header$mr$ti, x$header$mr$flip_angle_degrees, x$header$mr$fov));
  } else {
    cat(sprintf(" - Header does not contain MR acquisition parameters.\n"));
  }
}
