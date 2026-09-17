# TrackVis (TRK) affine handling -------------------------------------------------
#
# TRK files store streamlines in "voxelmm" space, where the coordinates refer to
# the corner of a voxel, and a matrix that maps them to RAS+ space. The matrix is
# only the affine for files whose voxels are 1 mm^3 and whose voxel order matches
# the orientation implied by the matrix, so computing the actual voxel-to-RAS mm
# affine requires the voxel sizes, the voxel order and the dimensions as well.
#
# The functions below reimplement the reference implementation from 'nibabel'
# (nibabel/streamlines/trk.py, get_affine_trackvis_to_rasmm(), and the
# orientation helpers in nibabel/orientations.py), which is what DIPY and the TRX
# tooling use as well:
#
#   affine = voxel_to_rasmm @ M @ translate(-0.5) @ scale(1 / voxel_sizes)
#
# where M is the axis permutation and flip that maps the voxel order stored in
# the header onto the voxel order implied by the affine.


#' @title Determine the orientation of an affine's input axes.
#'
#' @description For each of the 3 input (voxel) axes, determines which output
#'   (world) axis it corresponds to and whether it is flipped. This is the
#'   simplified equivalent of \code{nibabel.orientations.io_orientation()}: the
#'   dominant output axis of every input axis is used, with output axes removed
#'   from consideration once they have been assigned. nibabel additionally
#'   applies a polar decomposition first, which only matters for affines that
#'   contain shear.
#'
#' @param affine 4x4 numeric matrix.
#'
#' @return numeric matrix with 3 rows and 2 columns, as returned by
#'   \code{\link{io.orientation}}. Row i holds the 0-based output axis index and
#'   the direction (+1 or -1) of input axis i.
#'
#' @keywords internal
io.orientation <- function(affine) {
  affine <- as.matrix(affine);
  rotation <- affine[1:3, 1:3, drop = FALSE];

  # Process input axes from strongest to weakest, so that a given output axis is
  # assigned to the input axis it is most aligned with.
  strength <- apply(rotation, 2L, function(col) return(-max(col^2)));
  input_axes <- order(strength, method = "radix");

  ornt <- matrix(NA_real_, nrow = 3L, ncol = 2L);
  for (in_ax in input_axes) {
    column <- rotation[, in_ax];
    if (all(abs(column) < 1e-8)) {
      next; # Dropped axis.
    }
    out_ax <- which.max(abs(column));
    ornt[in_ax, 1L] <- out_ax - 1L;
    ornt[in_ax, 2L] <- if (column[out_ax] < 0) -1 else 1;
    rotation[out_ax, ] <- 0; # This output axis is taken now.
  }

  return(ornt);
}


#' @title Convert an orientation array to axis codes.
#'
#' @param ornt numeric matrix with 2 columns, see \code{\link{io.orientation}}.
#'
#' @return character vector with one code per input axis, drawn from
#'   \code{c('L','R','P','A','I','S')}. Dropped axes become NA.
#'
#' @keywords internal
ornt2axcodes <- function(ornt) {
  labels <- list(c("L", "R"), c("P", "A"), c("I", "S"));
  codes <- rep(NA_character_, nrow(ornt));
  for (row_idx in seq_len(nrow(ornt))) {
    axis_number <- ornt[row_idx, 1L];
    direction <- ornt[row_idx, 2L];
    if (is.na(axis_number) || is.na(direction)) {
      next;
    }
    codes[row_idx] <- if (direction == 1) labels[[axis_number + 1L]][2L] else labels[[axis_number + 1L]][1L];
  }
  return(codes);
}


#' @title Convert axis codes to an orientation array.
#'
#' @param axcodes character vector with one code per input axis, e.g.
#'   \code{c('L','P','S')} for a file whose first voxel axis points left.
#'
#' @return numeric matrix with 3 rows and 2 columns, see
#'   \code{\link{io.orientation}}.
#'
#' @keywords internal
axcodes2ornt <- function(axcodes) {
  labels <- list(c("L", "R"), c("P", "A"), c("I", "S"));
  ornt <- matrix(NA_real_, nrow = length(axcodes), ncol = 2L);
  for (code_idx in seq_along(axcodes)) {
    code <- axcodes[code_idx];
    for (label_idx in seq_along(labels)) {
      codes <- labels[[label_idx]];
      if (code %in% codes) {
        ornt[code_idx, ] <- c(label_idx - 1L, if (identical(code, codes[1L])) -1 else 1);
        break;
      }
    }
  }
  return(ornt);
}


#' @title Determine the orientation transform between two orientations.
#'
#' @param start_ornt numeric matrix with 2 columns, the initial orientation.
#'
#' @param end_ornt numeric matrix with 2 columns, the target orientation.
#'
#' @return numeric matrix with 2 columns, the orientation that maps
#'   \code{start_ornt} onto \code{end_ornt}.
#'
#' @keywords internal
ornt.transform <- function(start_ornt, end_ornt) {
  start_ornt <- as.matrix(start_ornt);
  end_ornt <- as.matrix(end_ornt);
  if (nrow(start_ornt) != nrow(end_ornt)) {
    stop("The orientations must have the same number of axes.\n");
  }

  result <- matrix(NA_real_, nrow = nrow(start_ornt), ncol = 2L);
  for (end_in_idx in seq_len(nrow(end_ornt))) {
    end_out_idx <- end_ornt[end_in_idx, 1L];
    assigned <- FALSE;
    for (start_in_idx in seq_len(nrow(start_ornt))) {
      if (!is.na(start_ornt[start_in_idx, 1L]) && start_ornt[start_in_idx, 1L] == end_out_idx) {
        flip <- if (start_ornt[start_in_idx, 2L] == end_ornt[end_in_idx, 2L]) 1 else -1;
        result[start_in_idx, ] <- c(end_in_idx - 1L, flip);
        assigned <- TRUE;
        break;
      }
    }
    if (!assigned) {
      stop(sprintf("Unable to find output axis %d in the orientation.\n", end_out_idx));
    }
  }

  return(result);
}


#' @title Build the affine that undoes an orientation transform.
#'
#' @description This is the equivalent of
#'   \code{nibabel.orientations.inv_ornt_aff()}: given an orientation and the
#'   shape of the array it was applied to, it returns the affine that maps
#'   coordinates in the transformed space back to coordinates in the original
#'   space.
#'
#' @param ornt numeric matrix with 2 columns, the orientation.
#'
#' @param shape numeric vector, the shape (dimensions) of the array.
#'
#' @return a 4x4 numeric matrix.
#'
#' @keywords internal
inv.ornt.aff <- function(ornt, shape) {
  ornt <- as.matrix(ornt);
  if (any(is.na(ornt))) {
    stop("Cannot invert an orientation that contains dropped axes.\n");
  }

  num_axes <- nrow(ornt);
  shape <- as.numeric(shape)[seq_len(num_axes)];

  # ornt implies a flip followed by a transpose; this undoes the transpose first
  # and the flip second.
  axis_transpose <- as.integer(ornt[, 1L]);
  identity_matrix <- diag(num_axes + 1L);
  undo_reorder <- identity_matrix[c(axis_transpose + 1L, num_axes + 1L), , drop = FALSE];

  undo_flip <- diag(c(ornt[, 2L], 1.0), nrow = num_axes + 1L);
  center_translation <- -(shape - 1) / 2.0;
  undo_flip[seq_len(num_axes), num_axes + 1L] <- (ornt[, 2L] * center_translation) - center_translation;

  return(undo_flip %*% undo_reorder);
}


#' @title Compute the affine that maps TrackVis coordinates to RAS+ mm.
#'
#' @description Computes the affine that turns the streamlines stored in a TRK
#'   file into RAS+ mm coordinates in which a coordinate of (0,0,0) is the center
#'   of the first voxel, which is the convention used by 'nibabel', DIPY and the
#'   TRX format. The stored streamlines refer to the voxel corners and are in
#'   "voxelmm" space, so this combines the stored matrix with the voxel sizes,
#'   the half-voxel offset, and the orientation stored in the \code{voxel_order}
#'   header field.
#'
#' @param header named list, the header of a TRK file as returned by
#'   \code{\link{read.dti.trk.header}}.
#'
#' @return a 4x4 numeric matrix.
#'
#' @note The TrackVis specification assumes that the stored streamlines are in
#'   mm and that the stored matrix maps them to RAS, which means the matrix has
#'   to be corrected whenever the voxels are not 1 mm^3. DSI Studio writes files
#'   that do not need the half-voxel shift, which is what the \code{shift_origin}
#'   parameter of \code{\link{read.dti.trk}} controls.
#'
#' @keywords internal
trackvis.affine.to.rasmm <- function(header) {
  voxel_to_rasmm <- header$vox2ras;
  voxel_sizes <- header$voxel_size;
  dimensions <- header$dim;
  voxel_order <- header$voxel_order;

  if (is.null(voxel_to_rasmm)) {
    stop("The TRK header does not contain a voxel-to-RAS matrix.\n");
  }
  if (is.null(voxel_sizes) || any(!is.finite(voxel_sizes)) || any(voxel_sizes == 0)) {
    stop("The TRK header does not contain usable voxel sizes.\n");
  }
  if (any(is.na(voxel_to_rasmm[4, ])) || voxel_to_rasmm[4, 4] == 0) {
    stop("The TRK header contains no usable voxel-to-RAS matrix (its last diagonal entry is 0).\n");
  }

  # voxelmm -> voxel.
  scale_matrix <- diag(4L);
  diag(scale_matrix)[1:3] <- 1.0 / voxel_sizes;

  # TrackVis considers (0,0,0) to be a voxel corner, whereas the returned
  # streamlines assume it to be the center of the voxel.
  offset_matrix <- diag(4L);
  offset_matrix[1:3, 4L] <- -0.5;

  affine <- offset_matrix %*% scale_matrix;

  # If the voxel order in the header does not match the orientation implied by
  # the affine, insert the permutation and flips that reconcile them.
  if (!is.null(voxel_order) && !is.na(voxel_order) && nzchar(trimws(voxel_order))) {
    header_orientation <- axcodes2ornt(strsplit(trimws(voxel_order), "")[[1L]]);
    affine_orientation <- axcodes2ornt(ornt2axcodes(io.orientation(voxel_to_rasmm)));
    reconcile <- ornt.transform(header_orientation, affine_orientation);
    affine <- inv.ornt.aff(reconcile, dimensions) %*% affine;
  }

  return(voxel_to_rasmm %*% affine);
}


#' @title Apply an affine to a set of coordinates.
#'
#' @param coords numeric matrix with 3 columns, the coordinates.
#'
#' @param affine 4x4 numeric matrix.
#'
#' @return numeric matrix with 3 columns, the transformed coordinates.
#'
#' @keywords internal
apply.affine.to.coords <- function(coords, affine) {
  if (nrow(coords) == 0L) {
    return(coords);
  }
  return(sweep(coords %*% t(affine[1:3, 1:3, drop = FALSE]), 2L, affine[1:3, 4L], "+"));
}


#' @title Transform a bounding box with an affine.
#'
#' @description Returns the axis-aligned bounding box of the transformed box.
#'   For a linear transformation the extremes of the image of a box are attained
#'   at its corners, so transforming the 8 corners gives the exact bounding box
#'   of the transformed data.
#'
#' @param bbox numeric vector of length 6,
#'   \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}.
#'
#' @param affine 4x4 numeric matrix.
#'
#' @return numeric vector of length 6.
#'
#' @keywords internal
transform.bbox <- function(bbox, affine) {
  corners <- as.matrix(expand.grid(
    x = bbox[1:2], y = bbox[3:4], z = bbox[5:6]
  ));
  transformed <- apply.affine.to.coords(unname(corners), affine);
  return(coord.bbox(transformed));
}


#' @title Check whether a matrix is (close to) the identity.
#'
#' @param mat numeric matrix.
#'
#' @param tolerance numeric, the tolerance for the comparison.
#'
#' @return logical.
#'
#' @keywords internal
is.identity.matrix <- function(mat, tolerance = 1e-6) {
  return(isTRUE(all.equal(unname(as.matrix(mat)), diag(nrow(mat)), tolerance = tolerance)));
}
