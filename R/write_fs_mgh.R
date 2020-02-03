#' @title Write file in FreeSurfer MGH or MGZ format
#'
#' @description Write brain data to a file in FreeSurfer binary MGH or MGZ format.
#'
#' @param data matrix of numerical values. The brain data to write. Must be integers or doubles. (The data type is set automatically to MRI_INT for integers and MRI_FLOAT for doubles in the MGH header).
#'
#' @param filepath string. Full path to the output curv file. If this ends with ".mgz", the file will be written gzipped (i.e., in MGZ instead of MGH format).
#'
#' @param vox2ras_matrix 4x4 matrix. An affine transformation matrix for the RAS transform that maps voxel indices in the volume to coordinates, such that for y(i1,i2,i3) (i.e., a voxel defined by 3 indices in the volume), the xyz coordinates are vox2ras_matrix*[i1 i2 i3 1]. If no matrix is given (or a NULL value), the ras_good flag will be 0 in the file. Defaults to NULL.
#'
#' @param mr_params double vector of length four. The acquisition parameters, in order: tr, flipangle, te, ti. The unit for the three times is ms, the angle unit is radians. Defaults to c(0, 0, 0, 0) if omitted. Pass NULL if you do not want to write them at all.
#'
#' @param mri_dtype character string representing an MRI data type code or 'auto'. Valid strings are 'MRI_UCHAR' (1 byte unsigned integer), 'MRI_SHORT' (2 byte signed integer), 'MRI_INT' (4 byte singed integer) and 'MRI_FLOAT' (4 byte signed floating point). The default value `auto` will determine the data type from the type of the `data` parameter. Leave this alone if in doubt.
#'
#' @family morphometry functions
#'
#' @export
write.fs.mgh <- function(filepath, data, vox2ras_matrix = NULL, mr_params = c(0., 0., 0., 0.), mri_dtype='auto') {

    ret_list = list("header"=list());

    # Sanity checks for arguments
    if (! is.array(data)) {
        if(is.vector(data) || is.matrix(data)) {
            # it is a vector, reshape to array
            data = array(data, dim=c(length(data), 1, 1, 1));
        } else {
            stop("The 'data' argument must be an array, a vector or a matrix.");
        }
    }

    if(! is.null(mr_params)) {
      if(length(mr_params) != 4) {
          stop(sprintf("The mr_params must be a double vector of length 4 but length is %d.", length(mr_params)));
      }
      if(! is.double(mr_params)) {
        stop("The mr_params but be a double vector.");
      }
    }

    if(length(vox2ras_matrix) == 0) {
        ras_flag = 0L;
    } else {
        if(! is.matrix(vox2ras_matrix)) {
            stop("The 'vox2ras_matrix' argument must be a matrix.");
        }

        if(length(vox2ras_matrix) != 16 || nrow(vox2ras_matrix) != 4) {
            stop(sprintf("The 'vox2ras_matrix' argument must be a 4x4 matrix of length 16, but length is %d and nrow is %d.", length(vox2ras_matrix), nrow(vox2ras_matrix)));
        }
        ras_flag = 1L;
        ret_list$header$vox2ras_matrix = vox2ras_matrix;
    }

    ret_list$header$ras_flag = ras_flag;

    if(guess.filename.is.gzipped(filepath, gz_extensions=c(".mgz"))) {
        fh = gzfile(filepath, "wb");
    } else {
        fh = file(filepath, "wb", blocking = TRUE);
    }

    d = dim(data);
    num_dim = length(d);

    dim1 = d[1];
    dim2 = ifelse(num_dim >= 2, d[2], 1);
    dim3 = ifelse(num_dim >= 3, d[3], 1);
    num_frames = ifelse(num_dim >= 4, d[4], 1);

    writeBin(as.integer(1), fh, size = 4, endian = "big"); # version code, must be 1
    writeBin(as.integer(dim1), fh, size = 4, endian = "big");
    writeBin(as.integer(dim2), fh, size = 4, endian = "big");
    writeBin(as.integer(dim3), fh, size = 4, endian = "big");
    writeBin(as.integer(num_frames), fh, size = 4, endian = "big");

    ret_list$header$voldim = c(dim1, dim2, dim3, num_frames);

    if (num_dim >= 5) {
        stop(sprintf("The data must not have more than 4 dimensions, but it has %d.", num_dim));
    }

    # write data type
    MRI_UCHAR = 0L;
    MRI_INT = 1L;
    MRI_FLOAT = 3L;
    MRI_SHORT = 4L;

    if(mri_dtype == 'auto') {
      if(is.integer(data)) {
          dtype = MRI_INT;
      } else if(is.double(data)) {
        dtype = MRI_FLOAT;
      } else {
          stop(sprintf("Data type '%s' not supported. Try integer or double.\n", typeof(data)));
      }
    } else {
      if(!is.character(mri_dtype)) {
        stop("Parameter 'mri_dtype' must be a character string.");
      }
      dtype = translate.mri.dtype(mri_dtype); # convert string to integer dtype code
    }

    ret_list$header$dtype = dtype;

    writeBin(as.integer(dtype), fh, size = 4,  endian = "big");  # write dtype code

    dof = 0L;    # Unused, ignore
    writeBin(as.integer(dof), fh, size = 4, endian = "big");

    header_size_total = 256L;    # MGH uses a fixed header size.

    ras_flag_size = 2L;
    writeBin(as.integer(ras_flag), fh, size = ras_flag_size, endian = "big");
    if(ras_flag == 1L) {
        MdcD = vox2ras_matrix[1:3, 1:3];   # The upper left 3x3 part of the 4x4 vox2ras matrix
        delta = sqrt(colSums(MdcD ** 2));    # a 3x1 vector

        delta_tvec = rep(delta, 3);  # 3x3 matrix
        Mdc = as.vector(MdcD / delta_tvec);
        Pcrs_c = c(dim1/2, dim2/2, dim3/2, 1);
        Pxyz_c = vox2ras_matrix %*% Pcrs_c;
        Pxyz_c = Pxyz_c[1:3];

        writeBin(delta, fh, size = 4, endian = "big"); # 3x1 vector => 3x4 = 12 bytes
        writeBin(Mdc, fh, size = 4, endian = "big");  # 3x3matrix => 9x4 = 36 bytes
        writeBin(Pxyz_c, fh, size = 4, endian = "big"); # 3x1 matrix => 3x4 = 12 bytes
        used_space_RAS = as.integer(3*4 + 4*3*4); # 12 + 36 + 12 = 60 bytes
    } else {
        used_space_RAS = 0L;
    }

    # The header has a fixed size (data starts at a fixed index), so fill the rest with zeros.
    space_to_fill = header_size_total - ras_flag_size - used_space_RAS;
    writeBin(as.integer(rep.int(0, space_to_fill)), fh, size = 1L, endian = "big");

    # Write the data:
    dtype_bytes = mri_dtype_numbytes(dtype);

    ret_list$data = data;
    data_vector = as.vector(data);

    writeBin(data_vector, fh, size = dtype_bytes, endian = "big");

    # A footer follows the data, it contains the MR acquisition parameters
    if(! is.null(mr_params)) {
      writeBin(mr_params, fh, size = 4, endian = "big");
    }
    close(fh);
}


#' @title Get size of MRI dtype in bytes.
#'
#' @param mri_dtype_code integer, the MRI data type code. See \code{\link[freesurferformats]{translate.mri.dtype}}.
#'
#' @return integer, the number of bytes
#'
#' @keywords internal
mri_dtype_numbytes <- function(mri_dtype_code) {
  if(is.character(mri_dtype_code)) {
    mri_dtype_code = translate.mri.dtype(mri_dtype_code);
  }
  if(mri_dtype_code == 0L) {# MRI_UCHAR
    return(1L);
  } else if(mri_dtype_code == 1L) { # MRI_INT
    return(4L);
  } else if(mri_dtype_code == 3L) { # MRI_FLOAT
    return(4L);
  } else if(mri_dtype_code == 4L) { # MRI_SHORT
    return(2L);
  } else {
    stop("Invalid mri_dtype_code parameter.")
  }
}
