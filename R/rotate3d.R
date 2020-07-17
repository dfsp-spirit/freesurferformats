# I guess there are some standard R functions for these.

#' @title Rotate a 2D matrix in 90 degree steps.
#'
#' @param slice a 2D matrix
#'
#' @param degrees integer, must be a (positive or negative) multiple of 90
#'
#' @return 2D matrix, the rotated matrix
#'
#' @export
rotate2D <- function(slice, degrees=90) {
  if(length(dim(slice)) != 2L) {
    if(is.vector(slice)) {
      return(slice); # return vector as is
    } else {
      stop("Slice must be a 2D matrix.");
    }
  }
  degrees = as.integer(degrees %% 360L);
  if(!degrees %in% as.integer(c(0, 90, 180, 270))) {
    stop("Parameter 'degrees' must be a multiple of 90 (it can be negative).");
  }
  if(degrees == 0L) {
    return(slice);
  } else if(degrees == 270) {
    return(rotate90(slice, times=1L, clockwise=FALSE));
  } else if(degrees == 180) {
    return(rotate90(slice, times=2L));
  } else {  # 90
    return(rotate90(slice));
  }
}


#' @title Rotate 2D matrix clockwise in 90 degree steps.
#'
#' @param mtx a 2D matrix
#'
#' @param times integer, how often to rotate in 90 degree steps. Example: pass `3L` to rotate `270` degrees.
#'
#' @param clockwise logical, whether to rotate clockwise.
#'
#' @keywords internal
rotate90 <- function(mtx, times=1L, clockwise=TRUE) {
  for(i in seq_len(times)) {
    if(clockwise) {
      mtx = t(apply(mtx, 2, rev));
    } else {
      mtx = apply(t(mtx), 2, rev);
    }
  }
  return(mtx);
}


#' @title Rotate a 3D array in 90 degree steps.
#'
#' @description Rotate a 3D array in 90 degree steps along an axis. This leads to an array with different dimensions.
#'
#' @param volume a 3D image volume
#'
#' @param axis positive integer in range 1L..3L or an axis name, the axis to use.
#'
#' @param degrees integer, must be a (positive or negative) multiple of 90L.
#'
#' @return a 3D image volume, rotated around the axis. The dimensions may or may not be different from the input image, depending on the rotation angle.
#'
#' @family volume math
#'
#' @export
rotate3D <- function(volume, axis=1L, degrees=90L) {
  if(length(dim(volume)) != 3) {
    stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
  }
  axis = as.integer(axis);
  if(axis < 1L | axis > 3L) {
    stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
  }
  dim1 = dim(volume)[1];
  dim2 = dim(volume)[2];
  dim3 = dim(volume)[3];
  num_voxels = dim1 * dim2 * dim3;

  degrees = as.integer(degrees);

  if(abs(degrees) %in% c(90L, 270L)) {
    if(axis == 1L) {
      new_dim = c(dim1, dim3, dim2);
    } else if (axis == 2L) {
      new_dim = c(dim3, dim2, dim1);
    } else {
      new_dim = c(dim2, dim1, dim3);
    }
  } else {
    new_dim = dim(volume);
  }

  rotbrain = array(rep(0L, num_voxels), new_dim);

  if(axis == 1L) {
    for(ax1_idx in seq_len(dim1)) {
      rotbrain[ax1_idx,,] = rotate2D(volume[ax1_idx,,], degrees = degrees);
    }
  } else if (axis == 2L) {
    for(ax2_idx in seq_len(dim2)) {
      rotbrain[,ax2_idx,] = rotate2D(volume[,ax2_idx,], degrees = degrees);
    }
  } else {
    for(ax3_idx in seq_len(dim3)) {
      rotbrain[,,ax3_idx] = rotate2D(volume[,,ax3_idx], degrees = degrees);
    }
  }
  return(rotbrain);
}


#' @title Flip a 3D array along an axis.
#'
#' @description Flip the slice of an 3D array horizontally or vertically along an axis. This leads to an output array with identical dimensions.
#'
#' @param volume a 3D image volume
#'
#' @param axis positive integer in range 1L..3L or an axis name, the axis to use.
#'
#' @param how character string, one of 'horizontally' / 'h' or 'vertically' / 'v'. How to flip the 2D slices. Note that flipping *horizontally* means that the image will be mirrored along the central *vertical* axis.
#'
#' @return a 3D image volume, flipped around the axis. The dimensions are identical to the dimensions of the input image.
#'
#' @family volume math
#'
#' @export
flip3D <- function(volume, axis=1L, how='horizontally') {
  if(length(dim(volume)) != 3) {
    stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
  }
  axis = as.integer(axis);
  if(axis < 1L | axis > 3L) {
    stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
  }
  dim1 = dim(volume)[1];
  dim2 = dim(volume)[2];
  dim3 = dim(volume)[3];
  num_voxels = dim1 * dim2 * dim3;

  flipped_brain = array(rep(0L, num_voxels), dim(volume));

  if(axis == 1L) {
    for(ax1_idx in seq_len(dim1)) {
      flipped_brain[ax1_idx,,] = flip2D(volume[ax1_idx,,], how = how);
    }
  } else if (axis == 2L) {
    for(ax2_idx in seq_len(dim2)) {
      flipped_brain[,ax2_idx,] = flip2D(volume[,ax2_idx,], how = how);
    }
  } else {
    for(ax3_idx in seq_len(dim3)) {
      flipped_brain[,,ax3_idx] = flip2D(volume[,,ax3_idx], how = how);
    }
  }
  return(flipped_brain);
}


#' @title Flip a 2D matrix.
#'
#' @param slice a 2D matrix
#'
#' @param how character string, one of 'vertically' / 'v' or 'horizontally' / 'h'. Note that flipping *horizontally* means that the image will be mirrored along the central *vertical* axis. If `NULL` is passed, the passed value is returned unaltered.
#'
#' @return 2D matrix, the flipped matrix.
#'
#' @export
flip2D <- function(slice, how='horizontally') {
  if(is.null(how)) {
    return(slice);
  }

  if(how == 'vertically' | how == 'v') {
    axis = 2L;
  } else if(how == 'horizontally' | how == 'h') {
    axis = 1L;
  } else {
    stop("How must be one of 'vertically' or 'horizontally' (or NULL for noop).");
  }

  if(length(dim(slice)) != 2L) {
    if(is.vector(slice)) {
      return(slice); # return vector as is
    } else {
      stop("Slice must be a 2D matrix.");
    }
  }
  if(axis == 1L) {
    return(slice[nrow(slice):1,]);
  } else {
    return(slice[, ncol(slice):1]);
  }
}
