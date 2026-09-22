# Functions for reading connectome files.
#
# A connectome file is a CIFTI-2 file whose two matrix dimensions both hold brainordinates
# (`.dconn`) or parcels (`.pconn`), or one of each (`.pdconn`, `.dpconn`). The XML metadata
# is read by R/read_cifti_header.R and the data matrix by R/read_cifti.R; this file adds the
# interpretation of the two dimensions, i.e. the parcels or brainordinates the rows and
# columns of the matrix belong to, which a caller needs to make sense of the numbers. The
# writers are in R/write_cifti_connectome.R.


#' @title Read a CIFTI-2 connectome file.
#'
#' @description Read a CIFTI-2 file whose two matrix dimensions both describe
#'   brainordinates or parcels, i.e. a dense connectome (`.dconn`), a parcellated
#'   connectome (`.pconn`) or one of the mixed types (`.pdconn`, `.dpconn`). The result
#'   contains the matrix and the parcels or brainordinates its rows and columns belong to;
#'   this is what makes a connectome file usable, since the numbers alone do not say which
#'   pair of regions a value describes.
#'
#'   Both values of a connectome are stored in the file (the matrix is not symmetric on
#'   disk, and the diagonal is stored as well), so reading a `.pconn` gives a square matrix
#'   with the number of parcels as its number of rows and columns. A real `.dconn` (an HCP
#'   subject has 91,282 grayordinates, i.e. 8.3 billion values, 33 GB) can not be read into
#'   memory at all: use the `rows` and `columns` parameters to read only the part you need,
#'   which is a contiguous block of the file for each requested column.
#'
#' @param filepath character string, the path of a CIFTI-2 file, see \code{\link{read.cifti}}.
#'   An `fs.cifti` object (see \code{\link{read.cifti.header}}) or an `fs.cifti.data` object
#'   (see \code{\link{read.cifti}}) are accepted as well.
#'
#' @param rows integer vector or `NULL`, the indices of matrix dimension 0 to read, see
#'   \code{\link{read.cifti}}.
#'
#' @param columns integer vector or `NULL`, the indices of matrix dimension 1 to read. This
#'   is the way to read part of a file that is too large to read completely, see
#'   \code{\link{read.cifti}}.
#'
#' @return a named list with class 'fs.connectome':
#'   \itemize{
#'     \item 'data': numeric matrix, the connectome, with the parcels or brainordinates as
#'       the dimnames of its rows and columns (see \code{\link{cifti.dim.labels}}),
#'     \item 'parcel_names': character vector, the names of the parcels, or `NULL` if the
#'       file has no parcellated dimension,
#'     \item 'parcels': data.frame with one row per parcel (the columns 'index', 'name',
#'       'num_vertices' and 'num_voxels', see \code{\link{cifti.parcels}}), or `NULL`,
#'     \item 'parcels_dim': integer, the matrix dimension that holds the parcels (0 or 1),
#'       or `NA` if the file has none,
#'     \item 'grayordinates': data.frame with one row per brainordinate of the dense
#'       dimension (see \code{\link{cifti.grayordinates}}), or `NULL`,
#'     \item 'grayordinates_dim': integer, the matrix dimension that holds the dense
#'       brainordinates (0 or 1), or `NA` if the file has none,
#'     \item 'header': the `fs.cifti` metadata object, see \code{\link{read.cifti.header}}.
#'   }
#'   Note that the parcels and brainordinates describe *all* indices of the dimension they
#'   belong to, not only the ones selected with `rows` or `columns`.
#'
#' @examples
#' pconn_file <- system.file("extdata", "cifti", "tiny.pconn.nii", package = "freesurferformats")
#' conn <- read.fs.connectome.cifti(pconn_file)
#' dim(conn$data)
#' conn$parcel_names
#' conn$data[1:2, 1:2]
#'
#' @family cifti functions
#' @export
read.fs.connectome.cifti <- function(filepath, rows = NULL, columns = NULL) {
  x <- cifti.data.for.connectome(filepath, rows = rows, columns = columns)
  cii <- x$header
  dim_sizes <- as.integer(cii$matrix$dim_sizes)
  if (length(dim_sizes) != 2L) {
    stop(sprintf("CIFTI-2 file '%s' has %d matrix dimensions, a connectome file has 2.\n",
                 cii$filepath, length(dim_sizes)))
  }

  types <- vapply(c(0L, 1L), function(dim) {
    return(cifti.map.for.dim(cii, dim)$type)
  }, character(1L))
  connectome_types <- c("CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_PARCELS")
  if (!all(types %in% connectome_types)) {
    stop(sprintf(paste0("The matrix dimensions of CIFTI-2 file '%s' are of type '%s' and '%s', which is not a connectome ",
                        "file: both dimensions of a connectome describe brainordinates or parcels (like in a '.dconn' ",
                        "or '.pconn'). Use read.cifti() to read the data of this file.\n"),
                 cii$filepath, cifti.index.type.short(types[1L]), cifti.index.type.short(types[2L])))
  }

  parcellated_dims <- which(types == "CIFTI_INDEX_TYPE_PARCELS") - 1L
  parcels <- NULL
  parcel_names <- NULL
  parcels_dim <- NA_integer_
  if (length(parcellated_dims) > 0L) {
    parcels_dim <- as.integer(parcellated_dims[1L])
    parcels <- cifti.parcels(cii, parcels_dim)
    parcel_names <- as.character(parcels$name)
    if (length(parcellated_dims) == 2L) {
      other_names <- as.character(cifti.parcels(cii, 1L)$name)
      if (!identical(parcel_names, other_names)) {
        warning(sprintf(paste0("CIFTI-2 file '%s' has parcels in both matrix dimensions, but the two parcel lists differ: ",
                               "the dimensions hold %d and %d parcels, and the names do not match. A parcellated connectome ",
                               "has the same parcels in both dimensions, so this file is unusual. The parcels returned ",
                               "belong to matrix dimension %d.\n"),
                        cii$filepath, dim_sizes[1L], dim_sizes[2L], parcels_dim))
      }
    }
  }

  dense_dims <- which(types == "CIFTI_INDEX_TYPE_BRAIN_MODELS") - 1L
  grayordinates <- NULL
  grayordinates_dim <- NA_integer_
  if (length(dense_dims) > 0L) {
    grayordinates_dim <- as.integer(dense_dims[1L])
    grayordinates <- cifti.grayordinates(cii, grayordinates_dim)
  }

  result <- list(
    data = x$data,
    parcel_names = parcel_names,
    parcels = parcels,
    parcels_dim = parcels_dim,
    grayordinates = grayordinates,
    grayordinates_dim = grayordinates_dim,
    header = cii
  )
  class(result) <- "fs.connectome"
  return(result)
}


#' @title Print an fs.connectome instance.
#'
#' @param x an `fs.connectome` instance, see \code{\link{read.fs.connectome.cifti}}.
#'
#' @param ... ignored.
#'
#' @return the instance, invisibly.
#'
#' @family cifti functions
#' @export
print.fs.connectome <- function(x, ...) {
  print.fs.cifti(x$header)
  cat(sprintf("Connectome matrix: %s values of type '%s'.\n",
              paste(dim(x$data), collapse = " x "), typeof(x$data)))
  if (!is.na(x$parcels_dim)) {
    cat(sprintf("Parcels (matrix dimension %d): %d, first ones: %s\n", x$parcels_dim,
                length(x$parcel_names), paste(utils::head(x$parcel_names, 3L), collapse = ", ")))
  }
  if (!is.na(x$grayordinates_dim)) {
    cat(sprintf("Brainordinates (matrix dimension %d): %d in total, %d of them surface vertices.\n",
                x$grayordinates_dim, nrow(x$grayordinates),
                sum(x$grayordinates$model_type == "SURFACE")))
  }
  return(invisible(x))
}


#' @title Accept the input forms of a CIFTI-2 connectome.
#'
#' @param filepath character string, `fs.cifti` or `fs.cifti.data`, see
#'   \code{\link{read.fs.connectome.cifti}}.
#'
#' @param rows integer vector or `NULL`, the indices of matrix dimension 0 to read.
#'
#' @param columns integer vector or `NULL`, the indices of matrix dimension 1 to read.
#'
#' @return an `fs.cifti.data` object, see \code{\link{read.cifti}}.
#'
#' @keywords internal
cifti.data.for.connectome <- function(filepath, rows = NULL, columns = NULL) {
  if (inherits(filepath, "fs.cifti.data") || inherits(filepath, "fs.cifti")) {
    if (!is.null(rows) || !is.null(columns)) {
      stop(paste0("The parameters 'rows' and 'columns' select the part of a file to read, so they can only be used when ",
                  "reading from a file. Pass the path of the file instead of an fs.cifti or fs.cifti.data object if you ",
                  "want to read only a part of the data.\n"))
    }
    if (inherits(filepath, "fs.cifti.data")) {
      return(filepath)
    }
    return(read.cifti(filepath$filepath))
  }
  if (!is.character(filepath) || length(filepath) != 1L || is.na(filepath)) {
    stop(paste0("Parameter 'filepath' must be the path of a CIFTI-2 file, an fs.cifti object (see read.cifti.header()) ",
                "or an fs.cifti.data object (see read.cifti()).\n"))
  }
  return(read.cifti(filepath, rows = rows, columns = columns))
}
