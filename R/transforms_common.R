#' @title Create an fs.transform instance.
#'
#' @description An `fs.transform` is a linear transformation matrix together with the coordinate spaces that it
#' maps between. It is the common representation that all transformation file format readers and writers in this
#' package return and accept, so that a matrix read from a file always states what its numbers mean: a bare 4x4
#' matrix does not tell whether it maps voxel indices or world coordinates, in which direction it goes, or
#' whether the voxel indices are zero- or one-based.
#'
#' The `matrix` field always maps from the source to the target, i.e., applying it to a coordinate means
#' `target_coord = matrix \%*\% c(source_coord, 1)`, which is the same convention that
#' \code{\link{doapply.transform.mtx}} uses. The inverse of the transformation is never stored, see
#' \code{\link{invert.fs.transform}}.
#'
#' @param matrix 4x4 numerical matrix, the transformation matrix. Required.
#'
#' @param space_in character string, the kind of coordinates the matrix maps from, one of 'voxel' (voxel indices
#'   of a volume), 'ras' (world coordinates, right-anterior-superior in millimeters) or 'lps' (world coordinates,
#'   left-posterior-superior in millimeters, used by ITK and ITK-based tools like ANTs). Use `NA` if the file did
#'   not state the space.
#'
#' @param space_out character string, the kind of coordinates the matrix maps to, see `space_in`.
#'
#' @param voxel_base integer, either 0 or 1. The index of the first voxel in the coordinates the matrix consumes
#'   and produces, i.e. 1 for the convention used by FreeSurfer and tkregister, and 0 for the one used by
#'   NIfTI, FSL and ITK. Must be `NA` unless `space_in` or `space_out` is 'voxel', since the value is meaningless
#'   for world coordinates.
#'
#' @param src `NULL` or named list, the volume or template that the matrix maps from. See `volume.descriptor`.
#'
#' @param dst `NULL` or named list, the volume or template that the matrix maps to.
#'
#' @param format character string, the file format the transform was read from, one of 'lta', 'dat', 'xfm' or
#'   'fslmat'. Use `NA` if the transform was not read from a file.
#'
#' @param source `NULL` or character string, the path of the file the transform was read from.
#'
#' @param type `NULL` or character string, the transform type as stated by the file format (e.g. 'Linear' for an
#'   xfm file, or the numeric LTA type as a string). This is format-specific metadata and is not interpreted by
#'   the package.
#'
#' @param ... additional named fields to store in the transform, e.g. the parsed header of the file it was read
#'   from. They are not validated and are preserved for formats that carry extra metadata.
#'
#' @return an `fs.transform` instance.
#'
#' @keywords internal
fs.transform <- function(matrix, space_in = NA_character_, space_out = NA_character_, voxel_base = NA_integer_,
                         src = NULL, dst = NULL, format = NA_character_, source = NULL, type = NULL, ...) {
  tf <- list(
    "matrix" = matrix,
    "space_in" = space_in,
    "space_out" = space_out,
    "voxel_base" = voxel_base,
    "src" = src,
    "dst" = dst,
    "format" = format,
    "source" = source,
    "type" = type
  )
  tf <- c(tf, list(...))
  class(tf) <- c("fs.transform", "list")
  validate.fs.transform(tf)
  return(tf)
}


#' @title Check whether an object is a valid fs.transform instance.
#'
#' @description This checks the invariants that every `fs.transform` must fulfill: it is a named list with a
#' finite 4x4 numerical matrix, a known or unknown coordinate space for both sides, a voxel base that matches
#' those spaces, and volume descriptors that only contain supported entries. It is called automatically by the
#' constructor, so that readers, writers and user code can rely on the fields.
#'
#' @param tf the object to check.
#'
#' @return `TRUE` if `tf` is a valid fs.transform instance, and the function `stop`s with an explanatory error
#'   message otherwise.
#'
#' @keywords internal
validate.fs.transform <- function(tf) {
  if (!is.list(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance (a list), found %s.\n", class(tf)[1L]))
  }

  if (is.null(tf$matrix) || !is.matrix(tf$matrix) || !is.numeric(tf$matrix)) {
    stop("An fs.transform must have a 'matrix' field which is a numerical matrix, e.g., 4x4.\n")
  }
  if (nrow(tf$matrix) != 4L || ncol(tf$matrix) != 4L) {
    stop(sprintf("The 'matrix' field of an fs.transform must be a 4x4 matrix, found %d x %d.\n", nrow(tf$matrix), ncol(tf$matrix)))
  }
  if (any(!is.finite(tf$matrix))) {
    stop("The 'matrix' field of an fs.transform must not contain NA, NaN or infinite values.\n")
  }

  valid_spaces <- c("voxel", "ras", "lps")
  for (space_field in c("space_in", "space_out")) {
    space_value <- tf[[space_field]]
    if (is.null(space_value) || length(space_value) != 1L) {
      stop(sprintf("The '%s' field of an fs.transform must be a single character string or NA.\n", space_field))
    }
    if (!is.na(space_value) && !(space_value %in% valid_spaces)) {
      stop(sprintf("The '%s' field of an fs.transform must be one of %s (or NA for unknown), found '%s'.\n", space_field, paste(valid_spaces, collapse = ", "), space_value))
    }
  }

  uses_voxel_space <- any(c(tf$space_in, tf$space_out) %in% "voxel")
  if (is.null(tf$voxel_base) || length(tf$voxel_base) != 1L) {
    stop("The 'voxel_base' field of an fs.transform must be a single integer (0, 1 or NA).\n")
  }
  if (uses_voxel_space && (is.na(tf$voxel_base) || !(tf$voxel_base %in% c(0L, 1L)))) {
    stop("An fs.transform that maps voxel coordinates must state whether the first voxel is index 0 or 1 in the 'voxel_base' field.\n")
  }
  if (!uses_voxel_space && !is.na(tf$voxel_base)) {
    stop("The 'voxel_base' field of an fs.transform must be NA unless the transform maps voxel coordinates.\n")
  }

  for (side_field in c("src", "dst")) {
    side_value <- tf[[side_field]]
    if (is.null(side_value)) {
      next
    }
    if (!is.list(side_value)) {
      stop(sprintf("The '%s' field of an fs.transform must be NULL or a list, see 'volume.descriptor'.\n", side_field))
    }
    unsupported <- setdiff(names(side_value), c("path", "dim", "voxelsize", "vox2ras", "frame", "valid"))
    if (length(unsupported) > 0L) {
      stop(sprintf("The '%s' field of an fs.transform contains unsupported entries: %s.\n", side_field, paste(unsupported, collapse = ", ")))
    }
    if (!is.null(side_value$dim) && length(side_value$dim) < 3L) {
      stop(sprintf("The 'dim' entry of the '%s' field of an fs.transform must have at least 3 entries, found %d.\n", side_field, length(side_value$dim)))
    }
    if (!is.null(side_value$voxelsize) && length(side_value$voxelsize) != 3L) {
      stop(sprintf("The 'voxelsize' entry of the '%s' field of an fs.transform must have 3 entries, found %d.\n", side_field, length(side_value$voxelsize)))
    }
    if (!is.null(side_value$vox2ras) && (!is.matrix(side_value$vox2ras) || !all(dim(side_value$vox2ras) == c(4L, 4L)))) {
      stop(sprintf("The 'vox2ras' entry of the '%s' field of an fs.transform must be a 4x4 matrix.\n", side_field))
    }
    if (!is.null(side_value$frame) && !(side_value$frame %in% c("scanner", "tkreg", "fsl"))) {
      stop(sprintf("The 'frame' entry of the '%s' field of an fs.transform must be 'scanner', 'tkreg' or 'fsl', found '%s'.\n", side_field, side_value$frame))
    }
  }

  return(TRUE)
}


#' @title Describe a volume for an fs.transform.
#'
#' @description Transformation files often record the geometry of the volumes they relate, either completely (an
#' LTA file stores the voxel dimensions, the voxel sizes and the voxel-to-RAS direction vectors of both volumes)
#' or not at all. This helper turns that information into the `src` and `dst` descriptors of an `fs.transform`.
#'
#' The `frame` entry states which RAS coordinates the `vox2ras` matrix produces: 'scanner' means that the matrix
#' is used as given (this is what an LTA file records), and 'tkreg' means FreeSurfer tkregister coordinates,
#' which are RAS coordinates with an identity rotation and the origin at the center of the volume. A descriptor
#' may be partial, e.g. when only the target is known to be in tkregister space.
#'
#' @param path `NULL` or character string, the file name of the volume as recorded in the transform file.
#'
#' @param dim `NULL` or integer vector of length 3, the volume dimensions (number of voxels along each axis).
#'
#' @param voxelsize `NULL` or numerical vector of length 3, the size of a voxel in mm along each axis.
#'
#' @param xras `NULL` or numerical vector of length 3, the RAS direction of the first voxel axis, scaled by the
#'   voxel size. This is the 'xras' entry of the volume info section of an LTA file.
#'
#' @param yras `NULL` or numerical vector of length 3, the RAS direction of the second voxel axis, scaled by the
#'   voxel size.
#'
#' @param zras `NULL` or numerical vector of length 3, the RAS direction of the third voxel axis, scaled by the
#'   voxel size.
#'
#' @param cras `NULL` or numerical vector of length 3, the RAS coordinates of the **center** of the volume, i.e.
#'   of voxel index `dim/2`. This is not the RAS coordinate of the first voxel, and using it as the translation
#'   column of a voxel-to-RAS matrix is an error of half the field of view. The 'cras' entry of the volume info
#'   section of an LTA file follows this convention, as verified against `mri_info --cras`.
#'
#' @param vox2ras `NULL` or 4x4 numerical matrix, the transformation from voxel indices to RAS coordinates. If
#'   not given but `dim`, `xras`, `yras`, `zras` and `cras` are, the matrix is computed from them in the
#'   convention that \code{\link{mghheader.vox2ras}} implements for MGH headers: zero-based voxel indices and
#'   the origin at `cras - Mdc_scaled * (dim/2)`. Verified against `mri_info --vox2ras` on a real volume.

#' @param frame character string, either 'scanner', 'tkreg' or 'fsl', see the description.
#'
#' @param valid `NULL` or integer, the 'valid' flag of the volume info section of an LTA file, which states
#'   whether the recorded geometry could be used by FreeSurfer.
#'
#' @return `NULL` if no information was given, a named list describing the volume otherwise.
#'
#' @keywords internal
volume.descriptor <- function(path = NULL, dim = NULL, voxelsize = NULL, xras = NULL, yras = NULL, zras = NULL,
                              cras = NULL, vox2ras = NULL, frame = "scanner", valid = NULL) {
  if (!is.null(frame) && !(frame %in% c("scanner", "tkreg", "fsl"))) {
    stop(sprintf("Parameter 'frame' must be 'scanner', 'tkreg' or 'fsl', found '%s'.\n", frame))
  }

  desc <- list()
  if (!is.null(path)) {
    desc$path <- as.character(path)
  }
  if (!is.null(dim)) {
    desc$dim <- as.integer(dim)
  }
  if (!is.null(voxelsize)) {
    desc$voxelsize <- as.numeric(voxelsize)
  }
  has_direction <- (!is.null(xras) && !is.null(yras) && !is.null(zras) && !is.null(cras))
  if (is.null(vox2ras) && has_direction) {
    if (is.null(dim)) {
      warning("Cannot compute the voxel-to-RAS matrix of a volume without its dimensions ('dim'), the direction vectors are not enough.")
    } else {
      mdc_scaled <- cbind(as.numeric(xras), as.numeric(yras), as.numeric(zras))
      pxyz_0 <- as.numeric(cras) - (mdc_scaled %*% (as.numeric(dim[1:3]) / 2.0))
      vox2ras <- rbind(cbind(mdc_scaled, pxyz_0), c(0, 0, 0, 1))
    }
  }
  if (!is.null(vox2ras)) {
    if (!is.matrix(vox2ras) || !all(dim(vox2ras) == c(4L, 4L))) {
      stop("Parameter 'vox2ras' must be a 4x4 numerical matrix.\n")
    }
    desc$vox2ras <- vox2ras
  }
  if (length(desc) == 0L) {
    return(NULL)
  }
  desc$frame <- frame
  if (!is.null(valid)) {
    desc$valid <- as.integer(valid)
  }
  return(desc)
}


#' @title Check whether an object is an fs.transform instance.
#'
#' @param x any object.
#'
#' @return logical, whether `x` is an `fs.transform` instance.
#'
#' @examples
#' tf <- read.fs.transform(system.file("extdata", "talairach.xfm",
#'   package = "freesurferformats", mustWork = TRUE
#' ))
#' is.fs.transform(tf)
#' is.fs.transform("no transform")
#'
#' @family header coordinate space
#'
#' @export
is.fs.transform <- function(x) {
  return(inherits(x, "fs.transform"))
}


#' @title Invert a transformation.
#'
#' @description Compute the transformation that undoes the given one, i.e. the one that maps the target
#' coordinates back to the source coordinates. The source and target of the result are swapped, along with the
#' coordinate spaces, and the matrix is inverted. A transformation is never stored inverted, use this function
#' when you need the reverse mapping.
#'
#' @param tf an `fs.transform` instance.
#'
#' @return an `fs.transform` instance that maps from the target of `tf` back to its source.
#'
#' @note The format-specific fields of `tf` (e.g. the parsed header and the volume info of an LTA file, or the
#'   intensity of a register.dat file) describe the file that `tf` was read from and are therefore copied to the
#'   result unchanged. They are metadata, not part of the mapping.
#'
#' @examples
#' tf <- read.fs.transform(system.file("extdata", "talairach.xfm",
#'   package = "freesurferformats", mustWork = TRUE
#' ))
#' tf_back <- invert.fs.transform(tf)
#' tf_back$space_in # the spaces are swapped
#' max(abs(tf_back$matrix %*% tf$matrix - diag(4))) # the identity
#'
#' @family header coordinate space
#'
#' @export
invert.fs.transform <- function(tf) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  tf_det <- det(tf$matrix)
  if (is.na(tf_det) || abs(tf_det) <= .Machine$double.eps) {
    stop(sprintf("Cannot invert this transformation, its matrix is not invertible (determinant is %s).\n", tf_det))
  }

  result <- tf
  result$matrix <- solve(tf$matrix)
  result$space_in <- tf$space_out
  result$space_out <- tf$space_in
  result$src <- tf$dst
  result$dst <- tf$src
  return(result)
}


#' @title Print an fs.transform instance.
#'
#' @param x an `fs.transform` instance.
#'
#' @param ... ignored, only present for consistency with the generic.
#'
#' @return the transform `x`, invisibly.
#'
#' @examples
#' tf <- read.fs.transform(system.file("extdata", "talairach.lta",
#'   package = "freesurferformats", mustWork = TRUE
#' ))
#' print(tf)
#'
#' @family header coordinate space
#'
#' @export
print.fs.transform <- function(x, ...) {
  space_label <- function(space, voxel_base) {
    if (is.na(space)) {
      return("unknown coordinates")
    }
    if (space == "voxel") {
      if (is.na(voxel_base)) {
        return("voxel coordinates")
      }
      return(sprintf("voxel coordinates (%d-based)", voxel_base))
    }
    if (space == "ras") {
      return("RAS world coordinates")
    }
    return("LPS world coordinates")
  }

  volume_label <- function(desc) {
    if (is.null(desc)) {
      return("not recorded in the file")
    }
    parts <- character(0)
    if (!is.null(desc$path)) {
      parts <- c(parts, sprintf("'%s'", desc$path))
    }
    if (!is.null(desc$dim)) {
      parts <- c(parts, sprintf("%s voxels", paste(desc$dim, collapse = "x")))
    }
    if (!is.null(desc$voxelsize)) {
      parts <- c(parts, sprintf("voxelsize %s", paste(desc$voxelsize, collapse = "x")))
    }
    if (!is.null(desc$frame) && desc$frame == "tkreg") {
      parts <- c(parts, "tkregister space")
    }
    if (!is.null(desc$frame) && desc$frame == "fsl") {
      parts <- c(parts, "FSL world space")
    }
    if (length(parts) == 0L) {
      return("recorded without geometry")
    }
    return(paste(parts, collapse = ", "))
  }

  if (is.na(x$format)) {
    cat("<fs.transform> of unknown format\n")
  } else if (is.null(x$source)) {
    cat(sprintf("<fs.transform> of format '%s'\n", x$format))
  } else {
    cat(sprintf("<fs.transform> of format '%s', loaded from '%s'\n", x$format, x$source))
  }
  cat(sprintf(
    "Maps %s to %s.\n",
    space_label(x$space_in, x$voxel_base), space_label(x$space_out, x$voxel_base)
  ))
  cat(sprintf("Source: %s\n", volume_label(x$src)))
  cat(sprintf("Target: %s\n", volume_label(x$dst)))
  print(x$matrix)

  tf_det <- det(x$matrix)
  is_affine <- all(abs(x$matrix[4L, ] - c(0, 0, 0, 1)) < sqrt(.Machine$double.eps))
  cat(sprintf("Determinant %s.", format(tf_det, digits = 6, trim = TRUE)))
  if (is.na(tf_det) || abs(tf_det) <= .Machine$double.eps) {
    cat(" The matrix is not invertible.")
  }
  cat("\n")
  cat(sprintf("Translation part: %s.\n", paste(format(x$matrix[1:3, 4L], digits = 6, trim = TRUE), collapse = ", ")))
  if (!is_affine) {
    cat("Note: the last row of the matrix is not '0 0 0 1', this is not an affine transformation.\n")
  }
  if (is.na(x$space_in) || is.na(x$space_out)) {
    cat("Note: the coordinate spaces of this transform are unknown, see the fields 'space_in' and 'space_out'.\n")
  }
  return(invisible(x))
}


#' @title Summarize an fs.transform instance.
#'
#' @description Compute the properties of a transformation in a machine-readable form, for printing or for
#' further processing.
#'
#' @param object an `fs.transform` instance.
#'
#' @param ... ignored, only present for consistency with the generic.
#'
#' @return named list with the entries 'format', 'source', 'space_in', 'space_out', 'voxel_base', 'src', 'dst'
#'   (the corresponding fields of the transform), 'translation' (numerical vector of length 3, the last column
#'   of the matrix), 'determinant', 'is_affine' (logical, whether the last row of the matrix is '0 0 0 1') and
#'   'is_invertible' (logical).
#'
#' @examples
#' tf <- read.fs.transform(system.file("extdata", "register.dat",
#'   package = "freesurferformats", mustWork = TRUE
#' ))
#' summary(tf)$space_out
#'
#' @family header coordinate space
#'
#' @export
summary.fs.transform <- function(object, ...) {
  tf_det <- det(object$matrix)
  result <- list(
    "format" = object$format,
    "source" = object$source,
    "space_in" = object$space_in,
    "space_out" = object$space_out,
    "voxel_base" = object$voxel_base,
    "src" = object$src,
    "dst" = object$dst,
    "translation" = object$matrix[1:3, 4L],
    "determinant" = tf_det,
    "is_affine" = all(abs(object$matrix[4L, ] - c(0, 0, 0, 1)) < sqrt(.Machine$double.eps)),
    "is_invertible" = (!is.na(tf_det) && abs(tf_det) > .Machine$double.eps)
  )
  return(result)
}
