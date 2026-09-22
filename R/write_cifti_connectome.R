# The user-facing CIFTI-2 writers for connectomes and parcellated files: they take data in
# the form the package uses (a matrix of connectivity values, one value or time series per
# parcel) and build the axes for it. See R/write_cifti.R for the generic writer,
# R/write_cifti_axes.R for the axis builders and R/cifti_parcels.R for building a parcels
# axis from brain surface annotations.


#' @title Write a CIFTI-2 connectome file.
#'
#' @description Write a connectome matrix to a CIFTI-2 file: a dense connectome
#'   (`.dconn`), a parcellated connectome (`.pconn`), or one of the mixed types (`.pdconn`,
#'   `.dpconn`) whose two dimensions hold brainordinates and parcels. The file type follows
#'   from the axes (and has to match the file name, see \code{\link{write.cifti}}), and the
#'   mapping of the two dimensions has to be given: it cannot be derived from the matrix,
#'   and guessing it (e.g. from the size of the matrix) would silently produce a file whose
#'   rows and columns describe the wrong brain regions.
#'
#'   Use `template` for the normal case, i.e. to write a connectome of the brainordinates or
#'   parcels of an existing file: this is how a `.dconn` of a subject gets the mapping of
#'   that subject, and how a `.pconn` gets the parcels of the `.ptseries` it was computed
#'   from. A template with a single brainordinate dimension (a `.dscalar`, `.dtseries` or
#'   `.dlabel`, i.e. the files that actually exist for a subject) is accepted as well: its
#'   mapping is then used for both dimensions of the connectome, which is what those files
#'   describe. Use `axes` if the mapping has to be built, e.g. a parcels axis from
#'   annotations (see \code{\link{cifti.axis.parcels.from.annot}}).
#'
#'   The complete matrix is written: a connectome file stores both halves of the matrix and
#'   its diagonal, so the symmetry of the matrix is not exploited anywhere, and the matrix
#'   of a `.dconn` can be huge (33 GB for the 91,282 grayordinates of an HCP subject, which
#'   is more than any implementation can hold in memory). Writing such a file is not
#'   supported yet; a connectome of a reduced set of brainordinates can be written.
#'
#' @param filepath character string, the path of the file to write. The name should be one
#'   of `.dconn.nii`, `.pconn.nii`, `.pdconn.nii` or `.dpconn.nii` (the file type is derived
#'   from the axes, and the name has to agree with it).
#'
#' @param data numeric matrix, the connectome: the first matrix dimension (the rows) is
#'   CIFTI matrix dimension 0, which the axes describe. An `fs.connectome` instance (see
#'   \code{\link{read.fs.connectome.cifti}}) is accepted as well, in which case its data and
#'   its header are used.
#'
#' @param template character string (the path of a CIFTI-2 file), an `fs.cifti` or an
#'   `fs.cifti.data` object, or `NULL`. The axes of this file are used; a file with a single
#'   brainordinate dimension has that mapping used for both dimensions of the connectome.
#'
#' @param axes list of two axes (see \code{\link{cifti.header.from.axes}}) or a single axis
#'   that is used for both dimensions, or `NULL`. Use this instead of `template` to write a
#'   file with a mapping that is not in a file, e.g. a parcels axis built with
#'   \code{\link{cifti.axis.parcels.from.annot}}.
#'
#' @param metadata named character vector or named list, or `NULL`, the matrix metadata, see
#'   \code{\link{write.cifti}}. The default keeps the metadata of the template file.
#'
#' @return the axes that were written, invisibly.
#'
#' @examples
#' pconn_file <- system.file("extdata", "cifti", "tiny.pconn.nii", package = "freesurferformats")
#' conn <- read.fs.connectome.cifti(pconn_file)
#' out_file <- file.path(tempdir(), "written.pconn.nii")
#' # The values are squared, the mapping is the one of the original file:
#' write.fs.connectome.cifti(out_file, conn$data^2, template = pconn_file)
#' read.fs.connectome.cifti(out_file)$data[1:2, 1:2]
#' \dontshow{unlink(out_file)}
#'
#' \dontrun{
#' # The grayordinates of a subject are the same in all its dense files, so a dtseries
#' # can define the mapping of the dconn:
#' write.fs.connectome.cifti("sub-01_dconn.nii", connectivity_matrix,
#'                           template = "sub-01_task-rest_dtseries.nii")
#' }
#'
#' @family cifti functions
#' @export
write.fs.connectome.cifti <- function(filepath, data, template = NULL, axes = NULL, metadata = NULL) {
  if (inherits(data, "fs.connectome")) {
    if (is.null(template)) {
      template <- data$header
    }
    data <- data$data
  }
  if (!is.matrix(data) || !(is.numeric(data) || is.integer(data))) {
    stop(paste0("Parameter 'data' must be a numeric or integer matrix: a connectome has two matrix dimensions, the ",
                "brainordinates or parcels of its rows (matrix dimension 0) and those of its columns (matrix dimension 1).\n"))
  }

  template_cii <- NULL
  if (!is.null(template)) {
    template_cii <- cifti.header.of(template)
  }
  axes <- cifti.connectome.axes(axes = axes, template_cii = template_cii)

  dim_sizes <- unname(vapply(axes, cifti.axis.size, integer(1L)))
  if (!identical(as.integer(dim(data)), dim_sizes)) {
    stop(sprintf(paste0("The connectome matrix has the dimensions %s, but its axes describe a matrix of size %s. The rows of ",
                        "the matrix are matrix dimension 0 of the file, the columns are matrix dimension 1.%s\n"),
                 paste(dim(data), collapse = " x "), paste(dim_sizes, collapse = " x "),
                 if (identical(rev(as.integer(dim(data))), dim_sizes)) {
                   " The data look transposed, so pass t(data) if the two dimensions are swapped."
                 } else {
                   ""
                 }))
  }

  return(write.cifti(filepath, data, axes = axes, template = template_cii, metadata = metadata))
}


#' @title Write a CIFTI-2 parcellated map or series file.
#'
#' @description Write data that has one value (or one time series) per parcel to a
#'   parcellated CIFTI-2 file: a `.pscalar` (scalar maps, e.g. a mean value per region) or a
#'   `.ptseries` (a time series per region). The parcellated dimension of these file types
#'   is matrix dimension 1, and the order of the parcels is the order of the parcels axis,
#'   which comes from a template file or from a parcels axis that you build (see
#'   \code{\link{cifti.axis.parcels.from.annot}} for a parcellation of this package, e.g.
#'   the annotations of a FreeSurfer atlas).
#'
#'   The file type is derived from the file name: a name ending in `.pscalar.nii` gets a
#'   scalars dimension (the map names come from `map_names`), one ending in `.ptseries.nii`
#'   a series dimension (described by `start`, `step` and `unit`). Use
#'   \code{\link{write.cifti}} directly for the other parcellated types (`.pconn`, the
#'   mixed connectome types) or for a file with a non-standard name.
#'
#' @param filepath character string, the path of the file to write, ending in
#'   `.pscalar.nii` or `.ptseries.nii`.
#'
#' @param data numeric vector or matrix, the data: a vector with one value per parcel (a
#'   `.pscalar` with a single map), or a matrix with one row per map (for a `.pscalar`) or
#'   series point (for a `.ptseries`) and one column per parcel. The first matrix dimension
#'   is CIFTI matrix dimension 0, like for every other reader and writer of this package.
#'
#' @param template character string (the path of a CIFTI-2 file), an `fs.cifti` or an
#'   `fs.cifti.data` object, or `NULL`. The parcels of this file are used, so pass the
#'   parcellated file the data belongs to (e.g. a `.ptseries` or `.pconn` of the same
#'   subject).
#'
#' @param axes list of two axes or a single parcels axis (see
#'   \code{\link{cifti.axis.parcels}}), or `NULL`. Use this instead of `template` to write
#'   data for a parcellation that is not in a CIFTI-2 file, e.g. one built from annotations
#'   with \code{\link{cifti.axis.parcels.from.annot}}.
#'
#' @param map_names character vector or `NULL`, the names of the maps of a `.pscalar`. The
#'   number of names has to match the number of rows of the data. Without this, the map
#'   names of the template are kept if it is a `.pscalar` with as many maps as the data.
#'
#' @param start numeric, the value of the first series point of a `.ptseries`, see
#'   \code{\link{cifti.axis.series}}.
#'
#' @param step numeric, the difference between consecutive series points.
#'
#' @param unit character string, the unit of the series, one of 'SECOND', 'HERTZ', 'METER'
#'   or 'RADIAN'.
#'
#' @param metadata named character vector or named list, or `NULL`, the matrix metadata, see
#'   \code{\link{write.cifti}}. The default keeps the metadata of the template file.
#'
#' @return the axes that were written, invisibly.
#'
#' @examples
#' template <- system.file("extdata", "cifti", "tiny.ptseries.nii", package = "freesurferformats")
#' data <- matrix(seq_len(3 * 4), nrow = 4L) # 4 series points, 3 parcels
#' out_file <- file.path(tempdir(), "written.ptseries.nii")
#' write.fs.parcellated.cifti(out_file, data, template = template, step = 0.5)
#' dim(read.cifti(out_file)$data)
#' \dontshow{unlink(out_file)}
#'
#' @family cifti functions
#' @export
write.fs.parcellated.cifti <- function(filepath, data, template = NULL, axes = NULL, map_names = NULL,
                                       start = 0, step = 1, unit = "SECOND", metadata = NULL) {
  if (!is.character(filepath) || length(filepath) != 1L || is.na(filepath)) {
    stop("Parameter 'filepath' must be a character string.")
  }
  file_type <- cifti.file.type.for.extension(filepath)
  if (!(file_type %in% c(".pscalar.nii", ".ptseries.nii"))) {
    stop(sprintf(paste0("write.fs.parcellated.cifti() writes the parcellated file types '.pscalar.nii' (a map per parcel) ",
                        "and '.ptseries.nii' (a time series per parcel), and the file name decides which one is written; ",
                        "'%s' is neither. Use write.cifti() for the other parcellated types ('.pconn', '.pdconn', ...) or ",
                        "for a file with a non-standard name.\n"), basename(filepath)))
  }

  template_cii <- NULL
  template_axes <- NULL
  if (!is.null(template)) {
    template_cii <- cifti.header.of(template)
    template_axes <- cifti.axis.from.template(template_cii)
  }

  parcels_axis <- NULL
  other_axis <- NULL
  if (!is.null(axes)) {
    single_axis <- is.list(axes) && !is.null(axes$type)
    axes <- cifti.check.axes(if (single_axis) list(axes) else axes)
    if (length(axes) == 1L) {
      if (!identical(axes[[1L]]$type, "CIFTI_INDEX_TYPE_PARCELS")) {
        stop(sprintf(paste0("The axis has the type '%s', but a parcellated file stores parcels in a matrix dimension, see ",
                            "cifti.axis.parcels().\n"), cifti.index.type.short(axes[[1L]]$type)))
      }
      parcels_axis <- axes[[1L]]
    } else if (length(axes) == 2L) {
      if (!identical(axes[[2L]]$type, "CIFTI_INDEX_TYPE_PARCELS")) {
        stop(sprintf(paste0("The second axis describes matrix dimension 1, which holds the parcels in a parcellated file, but ",
                            "its type is '%s'. Use a parcels axis (see cifti.axis.parcels()) for matrix dimension 1.\n"),
                     cifti.index.type.short(axes[[2L]]$type)))
      }
      parcels_axis <- axes[[2L]]
      other_axis <- axes[[1L]]
    } else {
      stop(paste0("Parameter 'axes' must be the parcels axis alone (which is used for matrix dimension 1), or a list of ",
                  "two axes, one per matrix dimension, see write.cifti().\n"))
    }
  }
  if (is.null(parcels_axis)) {
    if (is.null(template_cii)) {
      stop(paste0("Either 'template' or 'axes' has to be given: the parcels of the file come from the template (a ",
                  "parcellated CIFTI-2 file, e.g. the '.ptseries' of the same subject) or from an axis that you build ",
                  "yourself, see cifti.axis.parcels.from.annot().\n"))
    }
    parcels_axis <- cifti.parcels.axis.of(template_cii)
  }
  n_parcels <- cifti.axis.size(parcels_axis)

  if (is.null(dim(data))) {
    if (!is.numeric(data) && !is.integer(data)) {
      stop("Parameter 'data' must be numeric or integer, see the description of the layout.")
    }
    if (length(data) != n_parcels) {
      stop(sprintf(paste0("The data have %d values, but the parcels axis describes %d parcels. A vector is interpreted as ",
                          "one value per parcel.\n"), length(data), n_parcels))
    }
    data <- matrix(as.numeric(data), nrow = 1L)
  }
  if (length(dim(data)) != 2L || !(is.numeric(data) || is.integer(data))) {
    stop("Parameter 'data' must be a numeric vector or a 2-dimensional numeric matrix (maps or series points x parcels).")
  }
  if (ncol(data) != n_parcels) {
    stop(sprintf(paste0("The data have %d rows and %d columns, but the parcels axis describes %d parcels, which have to be ",
                        "the columns.%s\n"), nrow(data), ncol(data), n_parcels,
                 if (nrow(data) == n_parcels) {
                   " The data look transposed, so pass t(data): the first matrix dimension of a CIFTI-2 file is matrix dimension 0."
                 } else {
                   ""
                 }))
  }

  if (is.null(other_axis)) {
    if (identical(file_type, ".pscalar.nii")) {
      template_map_axis <- NULL
      if (!is.null(template_axes) && identical(template_axes[[1L]]$type, "CIFTI_INDEX_TYPE_SCALARS")) {
        template_map_axis <- template_axes[[1L]]
      }
      other_axis <- cifti.axis.for.maps(map_names, nx = nrow(data), template_map_axis = template_map_axis)
    } else {
      other_axis <- cifti.axis.series(nrow(data), start = start, step = step, unit = unit)
    }
  }

  return(write.cifti(filepath, data, axes = list(other_axis, parcels_axis), template = template_cii,
                     metadata = metadata))
}


# --- Internal helpers --------------------------------------------------------

#' @title The short name of a CIFTI-2 matrix index type.
#'
#' @param type character string, a CIFTI-2 matrix index type, see
#'   \code{\link{cifti.header.from.axes}}.
#'
#' @return character string, the type without the prefix, e.g. 'BRAIN_MODELS'.
#'
#' @keywords internal
cifti.index.type.short <- function(type) {
  return(sub("^CIFTI_INDEX_TYPE_", "", type))
}


#' @title The CIFTI-2 file type a file name names.
#'
#' @param filepath character string, a file name.
#'
#' @return character string, the standard file name extension the name ends with (e.g.
#'   '.pscalar.nii'), or the empty string if it does not end with one of them.
#'
#' @keywords internal
cifti.file.type.for.extension <- function(filepath) {
  file_types <- cifti.file.types()
  for (row_idx in seq_len(nrow(file_types))) {
    if (endsWith(filepath, file_types$extension[row_idx])) {
      return(file_types$extension[row_idx])
    }
  }
  return("")
}


#' @title Check whether an axis describes a connectome dimension.
#'
#' @param axis an axis, see \code{\link{cifti.header.from.axes}}.
#'
#' @return logical, whether the axis holds brainordinates or parcels.
#'
#' @keywords internal
cifti.is.connectome.axis <- function(axis) {
  return(axis$type %in% c("CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_PARCELS"))
}


#' @title Determine the two axes of a connectome file.
#'
#' @param axes list of one or two axes or `NULL`, see
#'   \code{\link{write.fs.connectome.cifti}}.
#'
#' @param template_cii an `fs.cifti` object or `NULL`, see
#'   \code{\link{write.fs.connectome.cifti}}.
#'
#' @return a list of two axes, named by matrix dimension.
#'
#' @keywords internal
cifti.connectome.axes <- function(axes, template_cii = NULL) {
  if (!is.null(axes)) {
    single_axis <- is.list(axes) && !is.null(axes$type)
    axes <- cifti.check.axes(if (single_axis) list(axes) else axes)
    if (length(axes) == 1L) {
      if (!cifti.is.connectome.axis(axes[[1L]])) {
        stop(sprintf(paste0("The axis has the type '%s', but a connectome dimension holds brainordinates or parcels. Use ",
                            "cifti.axis.brain.models() or cifti.axis.parcels() to build it.\n"),
                     cifti.index.type.short(axes[[1L]]$type)))
      }
      message(paste0("Using the same axis for both matrix dimensions of the connectome: a connectome has the same ",
                     "brainordinates or parcels in its rows and columns."))
      return(structure(list(axes[[1L]], axes[[1L]]), names = c("0", "1")))
    }
    if (length(axes) != 2L) {
      stop(paste0("Parameter 'axes' must be a list of two axes (one per matrix dimension), or a single brainordinate or ",
                  "parcel axis that is used for both dimensions.\n"))
    }
    return(axes)
  }

  if (is.null(template_cii)) {
    stop(paste0("Either 'template' or 'axes' has to be given: the mapping of a connectome cannot be derived from the matrix, ",
                "see write.fs.connectome.cifti().\n"))
  }
  template_axes <- cifti.axis.from.template(template_cii)
  if (length(template_axes) != 2L) {
    stop(sprintf("The template file '%s' has %d matrix dimensions, a connectome file has 2.\n",
                 template_cii$filepath, length(template_axes))) # nocov
  }
  is_connectome <- vapply(template_axes, cifti.is.connectome.axis, logical(1L))
  if (all(is_connectome)) {
    return(structure(template_axes, names = c("0", "1")))
  }
  if (any(is_connectome)) {
    axis <- template_axes[[which(is_connectome)[1L]]]
    message(sprintf(paste0("The template file has one brainordinate dimension (matrix dimension %d), and its mapping is used ",
                           "for both matrix dimensions of the connectome: the grayordinates or parcels of the rows and the ",
                           "columns of a connectome are the same."), which(is_connectome)[1L] - 1L))
    return(structure(list(axis, axis), names = c("0", "1")))
  }
  stop(sprintf(paste0("The template file '%s' has no brainordinate or parcel dimension (its matrix dimensions are of type ",
                      "'%s' and '%s'), so it cannot define the mapping of a connectome.\n"),
               template_cii$filepath, cifti.index.type.short(template_axes[[1L]]$type),
               cifti.index.type.short(template_axes[[2L]]$type)))
}


#' @title Get the parcels axis of a template file.
#'
#' @param template_cii an `fs.cifti` object, see \code{\link{cifti.header.of}}.
#'
#' @return the parcels axis of the file, see \code{\link{cifti.axis.parcels}}.
#'
#' @keywords internal
cifti.parcels.axis.of <- function(template_cii) {
  axes <- cifti.axis.from.template(template_cii)
  is_parcels <- vapply(axes, function(axis) identical(axis$type, "CIFTI_INDEX_TYPE_PARCELS"), logical(1L))
  if (!any(is_parcels)) {
    stop(sprintf(paste0("The template file '%s' has no parcels (its matrix dimensions are of type '%s' and '%s'), so it cannot ",
                        "define the parcels of the file to write. Use a parcellated file (e.g. a '.ptseries' or '.pconn') as the ",
                        "template, or build the parcels axis from annotations with cifti.axis.parcels.from.annot().\n"),
                 template_cii$filepath, cifti.index.type.short(axes[[1L]]$type),
                 cifti.index.type.short(axes[[length(axes)]]$type)))
  }
  return(axes[[which(is_parcels)[1L]]])
}
