#' @title Write a transformation matrix to a file.
#'
#' @description Save an `fs.transform` instance in one of the supported transformation file formats.
#'
#' A transformation stores its matrix together with the coordinate spaces it maps between, and the formats
#' disagree about which spaces they can express. A transformation is only written if the format can represent it
#' exactly, because a silent conversion would change the meaning of the matrix: FSL matrix files, for example,
#' store voxel-to-voxel matrices, so a transformation in world coordinates must be converted first with
#' \code{\link{transform.to.voxel}}. A format that cannot express the transformation at all is an error, not a
#' warning.
#'
#' @param tf an `fs.transform` instance, the transformation to write.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @param format character string, the file format, one of 'auto' (guess from the file extension), 'fslmat' (an
#'   FSL/FLIRT matrix file, i.e. a plain text 4x4 matrix as written by FSL's `flirt -omat`), 'lta'
#'   (\code{\link{write.fs.transform.lta}}), 'dat' (\code{\link{write.fs.transform.dat}}) or 'xfm'
#'   (\code{\link{write.fs.transform.xfm}}).
#'
#' @return the `fs.transform` instance `tf`, invisibly.
#'
#' @examples
#' tf <- read.fs.transform(system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE))
#' out_file <- tempfile(fileext = ".mat")
#' # An LTA of type 0 is a voxel-to-voxel transformation, so it can be written as an FSL matrix.
#' write.fs.transform(tf, out_file, format = "fslmat")
#' read.fs.transform(out_file)$matrix
#' unlink(out_file)
#'
#' # The same transformation can be written in the FreeSurfer formats of the spaces it maps between.
#' out_file <- tempfile(fileext = ".lta")
#' write.fs.transform(tf, out_file)
#' unlink(out_file)
#'
#' @family header coordinate space
#'
#' @export
write.fs.transform <- function(tf, filepath, format = "auto") {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }

  if (format == "auto") {
    format <- guess.writable.transform.format(filepath)
  }

  supported <- c("fslmat", "lta", "dat", "xfm")
  if (!(format %in% supported)) {
    stop(sprintf("Writing transformation files of format '%s' is not supported, supported formats are: %s.\n", format, paste(supported, collapse = ", ")))
  }

  if (format == "fslmat") {
    write.fs.transform.fslmat(tf, filepath)
  }
  if (format == "lta") {
    write.fs.transform.lta(tf, filepath)
  }
  if (format == "dat") {
    write.fs.transform.dat(tf, filepath)
  }
  if (format == "xfm") {
    write.fs.transform.xfm(tf, filepath)
  }
  return(invisible(tf))
}


#' @title Write a transformation matrix in FSL format.
#'
#' @description Write a 4x4 matrix as an FSL matrix file, i.e. as the plain text file that FSL's `flirt` writes
#' with the `-omat` option and that FSL, MRtrix3 and FreeSurfer read as the registration between two images. The
#' matrix must map voxel coordinates to voxel coordinates (`space_in` and `space_out` are 'voxel'), because that
#' is what an FSL matrix stores: it relates the voxel grid of the image given to `flirt -in` to the voxel grid of
#' the image given to `flirt -ref`, and it does not record which images those were. Use
#' \code{\link{transform.to.voxel}} to convert a transformation in world coordinates into one that can be
#' written.
#'
#' @param tf an `fs.transform` instance whose matrix maps voxel coordinates to voxel coordinates.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @return the `fs.transform` instance `tf`, invisibly.
#'
#' @examples
#' tf_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
#' out_file <- tempfile(fileext = ".mat")
#' write.fs.transform.fslmat(read.fs.transform(tf_file), out_file)
#' readLines(out_file)
#' unlink(out_file)
#'
#' @family header coordinate space
#'
#' @export
write.fs.transform.fslmat <- function(tf, filepath) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  if (!identical(tf$space_in, "voxel") || !identical(tf$space_out, "voxel")) {
    stop(sprintf("Cannot write this transformation as an FSL matrix: an FSL matrix maps voxel coordinates to voxel coordinates, but this one maps '%s' to '%s' coordinates. Use 'transform.to.voxel' to convert it first.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }
  if (!identical(tf$voxel_base, 0L)) {
    stop(sprintf("Cannot write this transformation as an FSL matrix: FSL voxel coordinates are zero-based (the first voxel is index 0), but this transformation uses the base %s. Use 'transform.to.voxel' to convert it first.\n", as.character(tf$voxel_base)))
  }

  matrix_lines <- transform.matrix.row.lines(tf$matrix)
  writeLines(matrix_lines, filepath)
  return(invisible(tf))
}


#' @title Determine the format of a transformation file to write.
#'
#' @description Guess the transformation file format from the file name extension, for the formats this package
#' can write.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @return character string, the file format.
#'
#' @keywords internal
guess.writable.transform.format <- function(filepath) {
  extension <- tolower(sub("^.*\\.", "", basename(filepath)))
  if (identical(extension, basename(filepath))) {
    extension <- "" # the file name contains no dot
  }
  if (extension == "mat") {
    # The '.mat' extension is used by FSL for its text matrices and by ANTs/ITK for binary transformations. The
    # package only writes the FSL format, and only text matrices are read, so this is unambiguous here.
    return("fslmat")
  }
  if (extension %in% c("lta", "dat", "xfm")) {
    return(extension)
  }
  stop(sprintf("Could not determine the transformation format to write for file '%s', please use the 'format' parameter.\n", filepath))
}


#' @title Write a FreeSurfer linear transform array (LTA) file.
#'
#' @description Write a transformation in the LTA format, which is the format FreeSurfer uses to exchange linear
#' transformations and that is read by `mri_vol2vol`, `tkregister2`, `mri_register` and `lta_convert`.
#'
#' The file states in its header whether the matrix operates on voxel indices (type 0, LINEAR_VOX_TO_VOX) or on
#' RAS coordinates (type 1, LINEAR_RAS_TO_RAS), so the spaces of the transformation determine the type that is
#' written. The geometry of the volumes is written from the `src` and `dst` descriptors, which makes the file
#' self-contained: FreeSurfer can convert it to other spaces without being given the volumes again.
#'
#' @param tf an `fs.transform` instance whose matrix maps either voxel coordinates to voxel coordinates, or RAS
#'   coordinates to RAS coordinates.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @return the `fs.transform` instance `tf`, invisibly.
#'
#' @note The `mean` and `sigma` header entries of an LTA file describe the registration that produced the matrix
#'   and are not used to interpret it. They are taken from the `header` field of `tf` if it has them, and are
#'   otherwise set to the center of the source volume and to 10000.
#'
#' @examples
#' lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
#' tf <- read.fs.transform(lta_file)
#' out_file <- tempfile(fileext = ".lta")
#' write.fs.transform.lta(tf, out_file)
#' max(abs(read.fs.transform(out_file)$matrix - tf$matrix)) # 0, the matrix survives the round trip
#' unlink(out_file)
#'
#' @family header coordinate space
#'
#' @export
write.fs.transform.lta <- function(tf, filepath) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }

  # The LTA type states what the matrix operates on, so it follows from the spaces of the transformation.
  lta_type <- NA_integer_
  if (!is.null(tf$type) && length(tf$type) == 1L && !is.na(tf$type)) {
    lta_type <- suppressWarnings(as.integer(tf$type))
  }
  if (is.na(lta_type)) {
    if (identical(tf$space_in, "voxel") && identical(tf$space_out, "voxel")) {
      lta_type <- 0L
    } else if (identical(tf$space_in, "ras") && identical(tf$space_out, "ras")) {
      lta_type <- 1L
    }
  }
  if (is.na(lta_type)) {
    stop(sprintf("Cannot write this transformation as an LTA file: its spaces are '%s' to '%s', and an LTA matrix maps either voxel coordinates to voxel coordinates or RAS coordinates to RAS coordinates. Use 'transform.to.voxel' or 'transform.to.world' to convert it first.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }
  if (lta_type == 0L && !(identical(tf$space_in, "voxel") && identical(tf$space_out, "voxel"))) {
    stop(sprintf("Cannot write this transformation as an LTA file of type 0 (LINEAR_VOX_TO_VOX): it maps '%s' to '%s' coordinates. Use 'transform.to.voxel' to convert it first, or write a type 1 file by removing the 'type' field.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }
  if (lta_type == 1L && !(identical(tf$space_in, "ras") && identical(tf$space_out, "ras"))) {
    stop(sprintf("Cannot write this transformation as an LTA file of type 1 (LINEAR_RAS_TO_RAS): it maps '%s' to '%s' coordinates. Use 'transform.to.world' to convert it first, or write a type 0 file by removing the 'type' field.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }

  mean_entry <- tf$header$mean
  if (is.null(mean_entry)) {
    if (!is.null(tf$src) && !is.null(tf$src$dim)) {
      mean_entry <- paste(sprintf("%.4f", tf$src$dim / 2.0), collapse = " ")
    } else {
      mean_entry <- "0.0000 0.0000 0.0000"
    }
  }
  sigma_entry <- tf$header$sigma
  if (is.null(sigma_entry)) {
    sigma_entry <- "10000.0000"
  }

  header_lines <- c(
    sprintf("# transform file %s", basename(filepath)),
    "# created by the freesurferformats package",
    "",
    sprintf("type      = %d # %s", lta_type, if (lta_type == 0L) "LINEAR_VOX_TO_VOX" else "LINEAR_RAS_TO_RAS"),
    "nxforms   = 1",
    sprintf("mean      = %s", mean_entry),
    sprintf("sigma     = %s", sigma_entry),
    "1 4 4"
  )

  matrix_lines <- transform.matrix.row.lines(tf$matrix)

  file_lines <- c(
    header_lines,
    matrix_lines,
    lta.volume.info.lines(tf$src, "src"),
    lta.volume.info.lines(tf$dst, "dst")
  )
  writeLines(file_lines, filepath)
  return(invisible(tf))
}


#' @title Write the volume info section of an LTA file.
#'
#' @description The volume info section records the geometry of one of the two volumes an LTA file relates:
#'   its dimensions, voxel sizes and the direction vectors and center that describe its RAS space. The geometry
#'   is written from a volume descriptor, and the direction vectors are the columns of its voxel-to-RAS matrix
#'   while the center is the RAS coordinate of voxel index `dim/2`, which is what FreeSurfer records there.
#'
#' @param descriptor `NULL` or a volume descriptor, see \code{\link{volume.descriptor}}.
#'
#' @param section_name character string, either 'src' or 'dst'.
#'
#' @return character vector, the lines of the section.
#'
#' @keywords internal
lta.volume.info.lines <- function(descriptor, section_name) {
  section_lines <- c(sprintf("%s volume info", section_name))
  if (is.null(descriptor) || is.null(descriptor$vox2ras) || is.null(descriptor$dim)) {
    section_lines <- c(section_lines, "valid = 0  # volume info not known")
    if (!is.null(descriptor) && !is.null(descriptor$path)) {
      section_lines <- c(section_lines, sprintf("filename = %s", descriptor$path))
    }
    return(section_lines)
  }

  vox2ras <- descriptor$vox2ras
  dims <- as.integer(descriptor$dim)[1:3]
  voxelsize <- descriptor$voxelsize
  if (is.null(voxelsize)) {
    voxelsize <- sqrt(colSums(vox2ras[1:3, 1:3]^2))
  }
  # This is the inverse of the construction in 'volume.descriptor': voxel index dim/2 maps to the center RAS.
  cras <- as.numeric((vox2ras %*% c(dims / 2.0, 1.0))[1:3])

  section_lines <- c(
    section_lines,
    "valid = 1  # volume info valid",
    sprintf("filename = %s", if (is.null(descriptor$path)) "unknown" else descriptor$path),
    sprintf("volume = %d %d %d", dims[1L], dims[2L], dims[3L]),
    sprintf("voxelsize = %s", transform.values.text(voxelsize)),
    sprintf("xras   = %s", transform.values.text(vox2ras[1:3, 1L])),
    sprintf("yras   = %s", transform.values.text(vox2ras[1:3, 2L])),
    sprintf("zras   = %s", transform.values.text(vox2ras[1:3, 3L])),
    sprintf("cras   = %s", transform.values.text(cras))
  )
  return(section_lines)
}


#' @title Format the rows of a transformation matrix for a text file.
#'
#' @description The transformation file formats store the matrix as text, and the values are written with 17
#' significant digits so that reading the file back gives the exact same double values. Fewer digits are not
#' enough: 15 digits, as used by some other tools, lose up to a few units in the last place of a double.
#'
#' @param matrix numerical matrix, the matrix to format.
#'
#' @return character vector with one entry per row of the matrix.
#'
#' @keywords internal
transform.matrix.row.lines <- function(matrix) {
  return(apply(matrix, 1L, function(matrix_row) transform.values.text(matrix_row)))
}


#' @title Format numerical values for a transformation text file.
#'
#' @param values numerical vector, the values to format.
#'
#' @return character string, the values separated by single spaces.
#'
#' @keywords internal
transform.values.text <- function(values) {
  return(paste(sprintf("%.17g", values), collapse = " "))
}


#' @title Write a tkregister dat file.
#'
#' @description Write a transformation in the FreeSurfer tkregister format (`register.dat`), the format that
#' `mri_vol2vol --reg`, `tkregister2` and `bbregister` use. Such a matrix maps the voxel coordinates of the
#' movable volume (the source) to RAS coordinates in the tkregister frame of the target volume, see
#' \code{\link{mghheader.vox2ras.tkreg}}, so a transformation can only be written if this is what it maps.
#'
#' @param tf an `fs.transform` instance whose matrix maps voxel coordinates to RAS coordinates.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @param subject `NULL` or character string, the subject identifier to store in the first line of the file. This
#'   is metadata for the tools that read the file and does not influence the transformation. If `NULL` and `tf`
#'   has a `subject` field (as read by \code{\link{read.fs.transform.dat}}), that value is used.
#'
#' @param in_plane_resolution `NULL` or numerical vector of length 2, the in-plane and between-plane resolution
#'   of the movable volume in millimeters, stored in the second and third line of the file. These are metadata
#'   that describe the acquisition and do not influence the transformation. If `NULL` and `tf` has the
#'   corresponding fields, they are used, otherwise they are derived from the geometry of the source volume, and
#'   set to 1 if that is not available either.
#'
#' @return the `fs.transform` instance `tf`, invisibly.
#'
#' @examples
#' dat_file <- system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE)
#' tf <- read.fs.transform(dat_file)
#' out_file <- tempfile(fileext = ".dat")
#' write.fs.transform.dat(tf, out_file)
#' max(abs(read.fs.transform(out_file)$matrix - tf$matrix)) # 0
#' unlink(out_file)
#'
#' @family header coordinate space
#'
#' @export
write.fs.transform.dat <- function(tf, filepath, subject = NULL, in_plane_resolution = NULL) {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  if (!identical(tf$space_in, "voxel") || !identical(tf$space_out, "ras")) {
    stop(sprintf("Cannot write this transformation as a tkregister dat file: such a matrix maps the voxel coordinates of the movable volume to the RAS coordinates of the target, but this transformation maps '%s' to '%s' coordinates.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }
  if (!identical(tf$voxel_base, 0L)) {
    stop(sprintf("Cannot write this transformation as a tkregister dat file: FreeSurfer voxel coordinates are zero-based (the first voxel is index 0), but this transformation uses the base %s.\n", as.character(tf$voxel_base)))
  }

  if (is.null(subject)) {
    subject <- if (is.null(tf$subject)) "unknown" else tf$subject
  }
  if (is.null(in_plane_resolution)) {
    if (!is.null(tf$in_plane_resolution) && !is.null(tf$between_plane_resolution)) {
      in_plane_resolution <- c(tf$in_plane_resolution, tf$between_plane_resolution)
    } else if (!is.null(tf$src) && !is.null(tf$src$voxelsize)) {
      in_plane_resolution <- c(tf$src$voxelsize[1L], tf$src$voxelsize[3L])
    } else {
      in_plane_resolution <- c(1.0, 1.0)
    }
  }
  intensity <- if (is.null(tf$intensity)) 0.15 else tf$intensity

  matrix_lines <- transform.matrix.row.lines(tf$matrix)
  writeLines(c(
    as.character(subject),
    transform.values.text(in_plane_resolution[1L]),
    transform.values.text(in_plane_resolution[2L]),
    transform.values.text(intensity),
    matrix_lines,
    "round"
  ), filepath)
  return(invisible(tf))
}


#' @title Write an MNI transform (xfm) file.
#'
#' @description Write a transformation in the FreeSurfer xfm format, the format of `talairach.xfm` files, which
#' store the transformation from the RAS space of a subject to the RAS space of an MNI or Talairach template.
#' Only the first three rows of the matrix are stored, so the transformation must be affine, and it must map RAS
#' coordinates to RAS coordinates.
#'
#' @param tf an `fs.transform` instance whose matrix maps RAS coordinates to RAS coordinates.
#'
#' @param filepath character string, the full path of the file to write.
#'
#' @param type character string, the value of the `Transform_Type` entry, 'Linear' by default.
#'
#' @return the `fs.transform` instance `tf`, invisibly.
#'
#' @examples
#' xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
#' tf <- read.fs.transform(xfm_file)
#' out_file <- tempfile(fileext = ".xfm")
#' write.fs.transform.xfm(tf, out_file)
#' max(abs(read.fs.transform(out_file)$matrix - tf$matrix)) # 0
#' unlink(out_file)
#'
#' @family header coordinate space
#'
#' @export
write.fs.transform.xfm <- function(tf, filepath, type = "Linear") {
  if (!is.fs.transform(tf)) {
    stop(sprintf("Parameter 'tf' must be an fs.transform instance, found %s.\n", class(tf)[1L]))
  }
  if (!identical(tf$space_in, "ras") || !identical(tf$space_out, "ras")) {
    stop(sprintf("Cannot write this transformation as an xfm file: an xfm stores a transformation between two RAS spaces, but this one maps '%s' to '%s' coordinates. Use 'transform.to.world' to convert it first.\n", as.character(tf$space_in), as.character(tf$space_out)))
  }
  frame <- transform.world.frame(tf)
  if (!identical(frame, "scanner")) {
    stop(sprintf("Cannot write this transformation as an xfm file: its coordinates are in the '%s' frame, which is not the RAS space of an image header that an xfm refers to.\n", frame))
  }
  if (!all(abs(tf$matrix[4L, ] - c(0, 0, 0, 1)) < sqrt(.Machine$double.eps))) {
    stop("Cannot write this transformation as an xfm file: the format stores only the first three rows of the matrix, i.e. it can only represent affine transformations, but the last row of this matrix is not '0 0 0 1'.\n")
  }

  matrix_lines <- transform.matrix.row.lines(tf$matrix[1:3, , drop = FALSE])
  matrix_lines[3L] <- sprintf("%s;", matrix_lines[3L]) # the last matrix line ends with a semicolon

  writeLines(c(
    "MNI Transform File",
    "% written by the freesurferformats package",
    "",
    sprintf("Transform_Type = %s;", type),
    "Linear_Transform =",
    matrix_lines
  ), filepath)
  return(invisible(tf))
}
