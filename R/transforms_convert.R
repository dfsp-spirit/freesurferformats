#' @title Get the geometry of a volume.
#'
#' @description Transformation matrices of the voxel-to-voxel kind can only be interpreted relative to the
#' volumes they relate, so the conversion functions need the geometry of those volumes: their dimensions, voxel
#' sizes and the matrix that maps voxel indices to RAS coordinates.
#'
#' The voxel indices are zero-based and the origin of the RAS space is the one used by FreeSurfer for MGH
#' headers and by NIfTI for the `sform`, i.e. this is the same convention as \code{\link{mghheader.vox2ras}}.
#'
#' @param volume an `fs.volume` instance (as returned by \code{\link{read.fs.volume}} or
#'   \code{\link{read.fs.mgh}}), a `nifti` instance (from the `oro.nifti` package), or a named list with the
#'   entries 'vox2ras_matrix', 'voldim' and 'voxelsize'.
#'
#' @return named list with the entries 'vox2ras' (4x4 numerical matrix), 'dim' (integer vector of length 3) and
#'   'voxelsize' (numerical vector of length 3).
#'
#' @keywords internal
volume.geometry <- function(volume) {
  if (is.fs.volume(volume)) {
    header <- volume$header
    if (is.null(header)) {
      stop("The 'fs.volume' instance does not carry a header, read it with 'with_header = TRUE'.\n")
    }
    if (!is.null(header$vox2ras_matrix)) {
      vox2ras <- header$vox2ras_matrix
    } else {
      vox2ras <- mghheader.vox2ras(header)
    }
    dims <- header$voldim
    if (is.null(dims)) {
      dims <- dim(volume$data)[1:3]
    }
  } else if (inherits(volume, "nifti")) {
    if (volume@sform_code <= 0L) {
      stop("The NIfTI instance has no 'sform' geometry (sform_code is 0), the volume geometry cannot be determined.\n")
    }
    vox2ras <- rbind(volume@srow_x, volume@srow_y, volume@srow_z, c(0, 0, 0, 1))
    dims <- dim(volume)[1:3]
  } else if (is.list(volume) && !is.null(volume$vox2ras_matrix)) {
    vox2ras <- volume$vox2ras_matrix
    dims <- volume$voldim
    if (is.null(dims)) {
      dims <- volume$dim # e.g. the header of a NIfTI file read by this package
    }
    if (is.null(dims)) {
      stop("The header has neither a 'voldim' nor a 'dim' entry, the volume geometry cannot be determined.\n")
    }
  } else {
    stop(sprintf("Cannot determine the geometry of parameter 'volume', which must be an fs.volume instance, a nifti instance or a header list, found %s.\n", class(volume)[1L]))
  }

  dims <- as.integer(dims)[1:3]
  if (length(dims) != 3L || any(is.na(dims))) {
    stop("Could not determine the 3 dimensions of the volume.\n")
  }
  if (!is.matrix(vox2ras) || !all(dim(vox2ras) == c(4L, 4L))) {
    stop("Could not determine a valid 4x4 voxel-to-RAS matrix for the volume.\n")
  }
  voxelsize <- sqrt(colSums(vox2ras[1:3, 1:3]^2))
  if (any(voxelsize <= 0.0)) {
    stop("Could not determine valid voxel sizes from the voxel-to-RAS matrix of the volume.\n")
  }

  return(list("vox2ras" = vox2ras, "dim" = dims, "voxelsize" = voxelsize))
}


#' @title Compute the matrix that maps FSL voxel coordinates of a volume to FSL world coordinates.
#'
#' @description FSL does not use the world coordinates of the image header. Its tools work in a space in which
#' the voxel axes have unit length (the voxel sizes are divided out) and in which the first voxel axis points to
#' the left, i.e. the transformation has a negative determinant, which is why FSL calls its images
#' 'radiological'. For a volume whose header already uses that convention, the FSL space equals the header space;
#' otherwise the first axis is flipped, and the origin of that axis is moved to the other end of the volume.
#'
#' This function implements the mapping that both MRtrix3 (in `transformconvert ... flirt_import`) and
#' FreeSurfer (in `lta_convert --infsl`) apply, and it was verified against both of them on real data: the
#' resulting transformation matrix is identical to the one of these tools up to numerical precision, while the
#' plain composition of the header voxel-to-RAS matrices is not (it differs by more than 100 mm on real data).
#'
#' @param geometry named list, the volume geometry as returned by \code{\link{volume.geometry}}.
#'
#' @return 4x4 numerical matrix, the transformation from (zero-based) voxel indices to FSL world coordinates.
#'
#' @keywords internal
fsl.scaled.voxel.matrix <- function(geometry) {
  # Divide the voxel sizes out of the header transform, keeping the translation.
  transformed <- geometry$vox2ras %*% diag(c(1.0 / geometry$voxelsize, 1.0))

  if (det(transformed[1:3, 1:3]) < 0.0) {
    return(transformed) # already in the FSL (radiological) convention
  }

  coord_switch <- diag(4)
  coord_switch[1L, 1L] <- -1.0
  coord_switch[1L, 4L] <- (geometry$dim[1L] - 1L) * geometry$voxelsize[1L]
  return(transformed %*% coord_switch)
}


#' @title Convert a transformation to the world (RAS) coordinate space.
#'
#' @description Transformation files often store their matrix in voxel coordinates, which means that the matrix
#' alone cannot be used to transform world coordinates (e.g. the coordinates of a brain surface vertex, or a
#' peak coordinate from another study): the geometry of the volumes the matrix relates is required as well. This
#' function converts such a transformation into one that operates on world coordinates.
#'
#' The result depends on the format the transformation was read from, because the formats disagree about their
#' world space, which is why this is not a pure matrix operation:
#'
#' * For an FSL matrix ('fslmat'), the world space is the one FSL uses: unit voxel axes with a flipped first
#'   axis, see \code{\link{fsl.scaled.voxel.matrix}}. Both MRtrix3 and FreeSurfer implement exactly this, and
#'   the result is a transformation between FSL world coordinates. The `src` and `dst` entries of the result
#'   have the frame 'fsl'.
#' * For an LTA of type 0 (VOX2VOX), the world space is the RAS space of the two volume geometries: the result
#'   is `vox2ras_dst \%*\% matrix \%*\% solve(vox2ras_src)`. The descriptors have the frame 'scanner'.
#'
#' @param tf an `fs.transform` instance with a matrix in voxel coordinates (`space_in` and `space_out` are
#'   'voxel'). A transformation that already operates on world coordinates is returned unchanged.
#'
#' @param src `NULL` or the volume the transformation maps from (the `-in` image of FSL, the `src` volume of an
#'   LTA). Required for FSL matrices, since their files do not record the volumes.
#'
#' @param dst `NULL` or the volume the transformation maps to (the `-ref` image of FSL, the `dst` volume of an
#'   LTA). Required for FSL matrices.
#'
#' @return an `fs.transform` instance whose matrix operates on world coordinates.
#'
#' @examples
#' # Read an LTA file, which records both volumes it relates, and convert it to world coordinates.
#' lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
#' tf <- read.fs.transform(lta_file)
#' tf_world <- transform.to.world(tf)
#' tf_world$space_in
#' tf_world$matrix
#'
#' @family header coordinate space
#'
#' @export
transform.to.world <- function(tf, src = NULL, dst = NULL) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  if (!identical(tf$space_in, "voxel") || !identical(tf$space_out, "voxel")) {
    return(tf) # nothing to do, it is already in a world space
  }

  src_geometry <- transform.geometry.for.side(tf, "src", src)
  dst_geometry <- transform.geometry.for.side(tf, "dst", dst)
  if (is.null(src_geometry) || is.null(dst_geometry)) {
    stop(sprintf("Cannot convert a '%s' transformation to world coordinates without the geometry of both volumes it relates: pass them as 'src' and 'dst' (see 'volume.geometry' for the accepted types).\n", as.character(tf$format)))
  }

  frame <- transform.world.frame(tf)
  if (frame == "fsl") {
    src_matrix <- fsl.scaled.voxel.matrix(src_geometry)
    dst_matrix <- fsl.scaled.voxel.matrix(dst_geometry)
  } else {
    src_matrix <- src_geometry$vox2ras
    dst_matrix <- dst_geometry$vox2ras
  }

  result <- tf
  result$matrix <- dst_matrix %*% tf$matrix %*% solve(src_matrix)
  result$space_in <- "ras"
  result$space_out <- "ras"
  result$voxel_base <- NA_integer_
  # The voxel sizes are derived from the recorded matrix so that the descriptor is consistent: in the FSL frame
  # the matrix has unit axes, i.e. a voxel size of 1, which also makes the FSL conversion idempotent (a matrix
  # in the FSL frame has a negative determinant and unit axes, so it is returned unchanged).
  result$src <- volume.descriptor(
    path = transform.descriptor.path(tf$src), dim = src_geometry$dim,
    voxelsize = sqrt(colSums(src_matrix[1:3, 1:3]^2)), vox2ras = src_matrix, frame = frame
  )
  result$dst <- volume.descriptor(
    path = transform.descriptor.path(tf$dst), dim = dst_geometry$dim,
    voxelsize = sqrt(colSums(dst_matrix[1:3, 1:3]^2)), vox2ras = dst_matrix, frame = frame
  )
  return(result)
}


#' @title Convert a transformation to voxel coordinates.
#'
#' @description The inverse operation of \code{\link{transform.to.world}}: given a transformation that operates
#' on world coordinates, compute the matrix that maps voxel indices of one volume to voxel indices of another.
#' The result can be saved as an FSL matrix, see \code{\link{write.fs.transform.fslmat}}.
#'
#' @inheritParams transform.to.world
#'
#' @return an `fs.transform` instance whose matrix operates on (zero-based) voxel coordinates.
#'
#' @examples
#' lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
#' tf_world <- transform.to.world(read.fs.transform(lta_file))
#' tf_voxel <- transform.to.voxel(tf_world)
#' max(abs(tf_voxel$matrix - read.fs.transform(lta_file)$matrix)) # back where we started
#'
#' @family header coordinate space
#'
#' @export
transform.to.voxel <- function(tf, src = NULL, dst = NULL) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  if (identical(tf$space_in, "voxel") && identical(tf$space_out, "voxel")) {
    return(tf) # nothing to do, it is already in voxel space
  }

  src_geometry <- transform.geometry.for.side(tf, "src", src)
  dst_geometry <- transform.geometry.for.side(tf, "dst", dst)
  if (is.null(src_geometry) || is.null(dst_geometry)) {
    stop(sprintf("Cannot convert a '%s' transformation to voxel coordinates without the geometry of both volumes it relates: pass them as 'src' and 'dst' (see 'volume.geometry' for the accepted types).\n", as.character(tf$format)))
  }

  src_frame <- transform.world.frame(tf)
  if (src_frame == "fsl") {
    src_matrix <- fsl.scaled.voxel.matrix(src_geometry)
    dst_matrix <- fsl.scaled.voxel.matrix(dst_geometry)
  } else {
    src_matrix <- src_geometry$vox2ras
    dst_matrix <- dst_geometry$vox2ras
  }

  result <- tf
  result$matrix <- solve(dst_matrix) %*% tf$matrix %*% src_matrix
  result$space_in <- "voxel"
  result$space_out <- "voxel"
  result$voxel_base <- 0L
  # The stored geometry is the one of the frame that the transformation is expressed in, so that converting
  # back to world coordinates later does not need the volumes again.
  result$src <- volume.descriptor(
    path = transform.descriptor.path(tf$src), dim = src_geometry$dim,
    voxelsize = sqrt(colSums(src_matrix[1:3, 1:3]^2)), vox2ras = src_matrix, frame = src_frame
  )
  result$dst <- volume.descriptor(
    path = transform.descriptor.path(tf$dst), dim = dst_geometry$dim,
    voxelsize = sqrt(colSums(dst_matrix[1:3, 1:3]^2)), vox2ras = dst_matrix, frame = src_frame
  )
  return(result)
}


#' @title Determine the volume geometry of one side of a transformation.
#'
#' @description Use the geometry recorded in the transformation itself if it is available, and the volume
#' passed by the caller otherwise.
#'
#' @param tf an `fs.transform` instance.
#'
#' @param side character string, either 'src' or 'dst'.
#'
#' @param volume `NULL` or a volume, see \code{\link{volume.geometry}}.
#'
#' @return `NULL` or a geometry list as returned by \code{\link{volume.geometry}}.
#'
#' @keywords internal
transform.geometry.for.side <- function(tf, side, volume = NULL) {
  if (!is.null(volume)) {
    return(volume.geometry(volume))
  }
  descriptor <- tf[[side]]
  if (!is.null(descriptor) && !is.null(descriptor$vox2ras) && !is.null(descriptor$dim)) {
    return(list("vox2ras" = descriptor$vox2ras, "dim" = descriptor$dim, "voxelsize" = descriptor$voxelsize))
  }
  return(NULL)
}


#' @title Get the file path recorded in a volume descriptor.
#'
#' @param descriptor `NULL` or a volume descriptor, see \code{\link{volume.descriptor}}.
#'
#' @return `NULL` or character string.
#'
#' @keywords internal
transform.descriptor.path <- function(descriptor) {
  if (is.null(descriptor) || is.null(descriptor$path)) {
    return(NULL)
  }
  return(descriptor$path)
}


#' @title Determine the frame of the world space of a transformation.
#'
#' @description The world space that a transformation between voxel coordinates refers to depends on the format
#' it was read from: an FSL matrix uses the FSL convention, see \code{\link{fsl.scaled.voxel.matrix}}, while the
#' matrices of the other formats use the RAS space of the volume headers. The frame is taken from the volume
#' descriptors if they state it and is derived from the format otherwise, so that
#' \code{\link{transform.to.world}} and \code{\link{transform.to.voxel}} always agree.
#'
#' @param tf an `fs.transform` instance.
#'
#' @return character string, either 'fsl' or 'scanner'.
#'
#' @keywords internal
transform.world.frame <- function(tf) {
  for (side in c("src", "dst")) {
    descriptor <- tf[[side]]
    if (!is.null(descriptor) && !is.null(descriptor$frame)) {
      return(descriptor$frame)
    }
  }
  if (!is.null(tf$format) && !is.na(tf$format) && identical(tf$format, "fslmat")) {
    return("fsl")
  }
  return("scanner")
}
