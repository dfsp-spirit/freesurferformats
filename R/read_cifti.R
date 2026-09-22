# Functions for reading the data values of CIFTI-2 files.
#
# The XML metadata of a CIFTI-2 file is read by the functions in R/read_cifti_header.R.
# This file deals with the data matrix, which is the NIFTI-2 payload that follows the
# header extension holding the XML. See dev_tools/project_plans/plan.md for the format
# notes the implementation is based on (section 2 of the CIFTI-2 spec).


#' @title Read a CIFTI-2 file.
#'
#' @description Read a CIFTI-2 file, i.e. its XML metadata (see
#'   \code{\link{read.cifti.header}}) and its data values. The data are returned
#'   as an array in the order in which they are stored in the file: the first
#'   array dimension is CIFTI matrix dimension 0 (which Connectome Workbench
#'   calls the ROW dimension), the second one is matrix dimension 1 (the COLUMN
#'   dimension). This is the same order that other CIFTI implementations use for
#'   their data arrays.
#'
#'   For a dense file like a `.dscalar`, dimension 0 holds the scalars (maps) and
#'   dimension 1 the brainordinates (surface vertices and volume voxels), so the
#'   data matrix has one row per map and one column per grayordinate. For a
#'   `.dtseries`, dimension 0 holds the series, so the matrix has one row per
#'   series point. The functions \code{\link{cifti.series.info}} and
#'   \code{\link{cifti.map.for.dim}} tell you what a dimension is; never conclude
#'   it from the file name. Use \code{\link{cifti.structure.data}} to extract the
#'   data of a brain structure, which is the more convenient interface for most
#'   purposes and the one that the \code{read.fs.*.cifti} functions are built on.
#'
#'   Reading a file twice (once for the header, once for the data) is not
#'   necessary to extract a single structure, but it is what this function does;
#'   use it if you need more than one structure or the raw matrix.
#'
#' @param filepath character string, the path to a CIFTI-2 file. Note that a
#'   CIFTI-2 file is a NIFTI-2 file, but its data are not a 3D volume; gzipped
#'   CIFTI files do not exist (the format forbids compression so that random
#'   access remains possible).
#'
#' @note The generic readers \code{\link{read.fs.morph}} and
#'   \code{\link{read.fs.volume}} dispatch on the file name and would therefore
#'   match a CIFTI-2 file (which is a NIFTI-2 file), so they detect this case and
#'   stop with a pointer to this function and the other CIFTI readers instead of
#'   returning the values of the matrix in an order that means nothing.
#'
#' @param rows integer vector or `NULL`, the indices of matrix dimension 0 to
#'   read. Indices are 1-based, like everywhere else in R, and they refer to the
#'   rows of the returned array, i.e. to the first CIFTI matrix dimension. Use
#'   `NULL` (the default) to read all of them.
#'
#' @param columns integer vector or `NULL`, the indices of matrix dimension 1 to
#'   read, i.e. the columns of the returned array. This is the matrix dimension
#'   that holds the brainordinates of a dense file (the grayordinates of a
#'   `.dscalar`, `.dtseries` or `.dlabel`), and it is also one of the two
#'   dimensions that hold them in a connectome file like a `.dconn`. Selecting
#'   columns avoids reading the rest of the file, which is the only practical way
#'   to read a small part of a huge file like a `.dconn` (9 to 38 GB): for a
#'   2-dimensional matrix, one contiguous block of the file holds all values of
#'   one column. Note that `rows` does not have this property, selecting rows
#'   still reads all values of the file.
#'
#' @return a named list with the entries 'header' (an `fs.cifti` object, see
#'   \code{\link{read.cifti.header}}) and 'data' (the numeric or integer array).
#'   The array dimensions are named with the axis labels of the corresponding
#'   matrix dimensions, see \code{\link{cifti.dim.labels}}. The object has class
#'   'fs.cifti.data'.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' cii <- read.cifti(cifti_file)
#' dim(cii$data)
#' # The first 3 grayordinates of the first map:
#' cii$data[1, 1:3]
#'
#' @family cifti functions
#' @export
read.cifti <- function(filepath, rows = NULL, columns = NULL) {
  cii <- read.cifti.header(filepath)
  data <- cifti.read.matrix(cii, rows = rows, columns = columns)
  result <- list(header = cii, data = data)
  class(result) <- "fs.cifti.data"
  return(result)
}


#' @title Print an fs.cifti.data instance.
#'
#' @param x an `fs.cifti.data` instance, see \code{\link{read.cifti}}.
#'
#' @param ... ignored.
#'
#' @return the instance, invisibly.
#'
#' @family cifti functions
#' @export
print.fs.cifti.data <- function(x, ...) {
  print.fs.cifti(x$header)
  dim_sizes <- dim(x$data)
  types <- vapply(seq_along(dim_sizes), function(dim) {
    return(sub("^CIFTI_INDEX_TYPE_", "", cifti.map.for.dim(x$header, dim - 1L)$type))
  }, character(1L))
  cat(sprintf("Data matrix: %s, type '%s' (matrix dimension %s).\n",
              paste(dim_sizes, collapse = " x "), typeof(x$data),
              paste(sprintf("%d: %s", seq_along(dim_sizes) - 1L, types), collapse = ", ")))
  for (dim in seq_along(dim_sizes)) {
    axis_labels <- dimnames(x$data)[[dim]]
    if (!is.null(axis_labels) && length(axis_labels) > 0L) {
      cat(sprintf("  Dimension %d labels (%d): %s\n", dim - 1L, length(axis_labels),
                  paste(utils::head(axis_labels, 3L), collapse = ", ")))
    }
  }
  return(invisible(x))
}


#' @title Extract the data of one brain structure from a CIFTI-2 file.
#'
#' @description Get the data values of a single brain structure (e.g. one
#'   hemisphere) from a CIFTI-2 file, reconstructed for the full surface. The
#'   result has one row per vertex of the surface (in the order of the surface,
#'   which is the order the vertices have in the surface mesh files) and one
#'   column per index of the other matrix dimension of the file. Vertices of the
#'   surface that have no value in the file are reported as `NA`: grayordinates
#'   files have a reduced mesh (the medial wall vertices are missing), and
#'   returning the values for the complete surface is what makes such files
#'   usable together with the standard surface meshes of a subject.
#'
#'   Volume structures (a brain model of type 'CIFTI_MODEL_TYPE_VOXELS', which
#'   subcortical structures use) cannot be expanded like this, because the voxels
#'   a structure consists of are not a rectangular block of a volume and are not
#'   ordered in any meaningful way. For those, the data values are returned
#'   together with the voxel indices and the affine transformation that maps them
#'   to coordinates, see the Value section.
#'
#' @param x an `fs.cifti.data` object, see \code{\link{read.cifti}}. An `fs.cifti`
#'   metadata object or the path of a CIFTI-2 file are accepted as well, in which
#'   case the data are read from the file.
#'
#' @param structure character string or `NULL`, the brain structure to extract
#'   the data for. The usual spellings are accepted ('lh', 'CORTEX_LEFT',
#'   'CIFTI_STRUCTURE_CORTEX_LEFT'), see
#'   \code{\link{cifti.structure.canonical}}. If it is `NULL`, the data of all
#'   structures are returned.
#'
#' @param dim integer or `NULL`, the CIFTI matrix dimension that holds the brain
#'   structure (the brainordinate dimension). For most files there is only one
#'   such dimension and this can be left at `NULL`. Files in which both
#'   dimensions are brain models (`.dconn` and `.pdconn`) require an explicit
#'   value, since they contain the same structures in both dimensions.
#'
#' @return If 'structure' is a single structure: a named list with the entries
#'   'structure' (character string, the canonical name of the structure, e.g.
#'   'CIFTI_STRUCTURE_CORTEX_LEFT'), 'structure_short' (the name without the
#'   prefix, e.g. 'CORTEX_LEFT'), 'model_type' (character string, one of
#'   'SURFACE', 'VOXELS' or 'SURFACE_AND_VOXELS'), 'surface' and 'volume'. If
#'   'structure' is `NULL`: a named list of such lists, one per structure, named
#'   by the short structure name.
#'
#'   For a selection that contains a surface model, the 'surface' entry holds a
#'   numeric (or integer) array with one row per vertex of the full surface and
#'   the remaining dimensions of the data, with `NA` for vertices that the file
#'   does not contain. Its dimensions beyond the first are named like in
#'   \code{\link{read.cifti}}. If the selection contains no surface model,
#'   'surface' is `NULL`.
#'
#'   For a selection that contains a volume model, the 'volume' entry holds a
#'   named list with the entries 'values' (an array like 'surface', but with one
#'   row per voxel of the structure instead of per surface vertex), 'voxel_indices_ijk'
#'   (an n x 3 integer matrix of 0-based voxel indices, in the same order as the
#'   rows of 'values'), 'dimensions' (integer vector of length 3, the dimensions
#'   of the volume the voxel indices refer to) and 'transformation_matrix' and
#'   'meter_exponent' (the 4x4 row-major matrix from the file that maps the voxel
#'   indices to coordinates in units of `10^meter_exponent`, and the exponent).
#'   If the selection contains no volume model, 'volume' is `NULL`.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' cii <- read.cifti(cifti_file)
#' lh_data <- cifti.structure.data(cii, "lh")
#' dim(lh_data$surface)
#' # vertices beyond the end of the mesh in the file are NA:
#' sum(is.na(lh_data$surface[, 1]))
#'
#' # Volume structures are returned with their voxel indices:
#' vol_file <- system.file("extdata", "cifti", "tiny_volume.dscalar.nii",
#'                         package = "freesurferformats")
#' vol_cii <- read.cifti(vol_file)
#' cerebellum <- cifti.structure.data(vol_cii, "CEREBELLUM")
#' cerebellum$volume$voxel_indices_ijk
#'
#' @family cifti functions
#' @export
cifti.structure.data <- function(x, structure = NULL, dim = NULL) {
  x <- cifti.data.object(x)
  cii <- x$header
  if (is.null(dim)) {
    dim <- cifti.brainordinate.dim(cii)
  }
  map <- cifti.map.for.dim(cii, dim)
  if (map$type != "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type '%s', not 'CIFTI_INDEX_TYPE_BRAIN_MODELS', so it does not describe brain structures. For parcellated data, see cifti.parcels().\n",
                 as.integer(dim), cii$filepath, map$type))
  }
  if (is.null(map$brain_models)) {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type 'CIFTI_INDEX_TYPE_BRAIN_MODELS' but contains no brain model entries.\n",
                 as.integer(dim), cii$filepath))
  }

  brainordinates <- cifti.grayordinates(cii, dim)
  # Move the brainordinate dimension to the front, so that all following code can
  # index the data by brainordinate.
  data <- cifti.dim.to.front(x$data, dim)

  if (is.null(structure)) {
    structures <- unique(brainordinates$structure)
    result <- lapply(structures, function(s) {
      return(cifti.structure.data.one(cii, map, brainordinates, data, s))
    })
    names(result) <- vapply(structures, function(s) cifti.structure.short(s), character(1L))
    return(result)
  }

  target <- cifti.structure.canonical(structure)
  if (!(target %in% brainordinates$structure)) {
    stop(sprintf("CIFTI-2 file '%s' contains no brain structure named '%s' in matrix dimension %d. Available structures: %s.\n",
                 cii$filepath, as.character(structure), as.integer(dim),
                 paste(unique(brainordinates$structure_short), collapse = ", ")))
  }
  return(cifti.structure.data.one(cii, map, brainordinates, data, target))
}


#' @title Extract the data of one brain structure (internal).
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param map the indices map of the brainordinate dimension, see
#'   \code{\link{cifti.map.for.dim}}.
#'
#' @param brainordinates the brainordinate table, see
#'   \code{\link{cifti.grayordinates}}.
#'
#' @param data the data array with the brainordinate dimension first, see
#'   \code{\link{cifti.dim.to.front}}.
#'
#' @param structure character string, the canonical name of the structure.
#'
#' @return a named list, see \code{\link{cifti.structure.data}}.
#'
#' @keywords internal
cifti.structure.data.one <- function(cii, map, brainordinates, data, structure) {
  selected <- which(brainordinates$structure == structure)
  model_types <- unique(brainordinates$model_type[selected])

  result <- list(
    structure = structure,
    structure_short = cifti.structure.short(structure),
    model_type = paste(model_types, collapse = "_AND_"),
    surface = NULL,
    volume = NULL
  )

  surface_rows <- selected[brainordinates$model_type[selected] == "SURFACE"]
  if (length(surface_rows) > 0L) {
    n_vertices <- cifti.surface.vertex.count(map, structure)
    if (is.na(n_vertices)) {
      stop(sprintf("CIFTI-2 file '%s': the brain models for structure '%s' are surfaces, but the file does not declare the number of vertices of the surface, so the data cannot be reconstructed. This makes the file invalid.\n",
                   cii$filepath, as.character(structure)))
    }
    # One row per vertex of the complete surface. A structure can have several
    # surface brain model entries (e.g. one group per hemisphere part or a split
    # range), all of them describe vertices of the same surface.
    surface_data <- cifti.array.with.new.first.dim(data, n_vertices)
    surface_data <- cifti.assign.rows(surface_data,
                                      to = brainordinates$vertex_index[surface_rows] + 1L,
                                      from = brainordinates$index[surface_rows] + 1L,
                                      data = data)
    dimnames(surface_data) <- c(list(NULL), dimnames(data)[-1L])
    result$surface <- surface_data
  }

  volume_rows <- selected[brainordinates$model_type[selected] == "VOXELS"]
  if (length(volume_rows) > 0L) {
    if (is.null(map$volumes)) {
      stop(sprintf("CIFTI-2 file '%s' contains volume brain models but no 'Volume' element, so the voxel indices cannot be interpreted. This makes the file invalid.\n",
                   cii$filepath)) # nocov
    }
    volume_info <- map$volumes[[1L]]
    volume_data <- do.call(`[`, c(list(data), c(list(brainordinates$index[volume_rows] + 1L),
                                               rep(list(TRUE), length(dim(data)) - 1L),
                                               list(drop = FALSE))))
    dimnames(volume_data) <- c(list(NULL), dimnames(data)[-1L])
    result$volume <- list(
      values = volume_data,
      voxel_indices_ijk = as.matrix(brainordinates[volume_rows, c("i", "j", "k")]),
      dimensions = volume_info$dimensions,
      meter_exponent = volume_info$meter_exponent,
      transformation_matrix = volume_info$transformation_matrix
    )
  }

  return(result)
}


#' @title Get the brainordinate table of a CIFTI-2 file.
#'
#' @description Get the mapping from the matrix indices of one dimension to the
#'   individual surface vertices and volume voxels a CIFTI-2 file contains. This
#'   is the per-index version of the table returned by
#'   \code{\link{cifti.structures}}, which reports index ranges instead of single
#'   indices. It is useful to find out which vertex or voxel a data row belongs
#'   to, or to find the data index of a vertex or voxel.
#'
#'   Note that the same vertex or voxel can only appear once per dimension (the
#'   matrix has one entry per brainordinate), but a structure can be split over
#'   several brain model entries, and the vertices of a grayordinates file are a
#'   subset of the vertices of the surface it refers to.
#'
#' @inheritParams cifti.structures
#'
#' @return a data.frame with one row per index of the matrix dimension and the
#'   columns 'index' (integer, 0-based matrix index), 'structure' (character
#'   string, the brain structure as spelled in the file), 'structure_short'
#'   (without the `CIFTI_STRUCTURE_` prefix), 'model_type' ('SURFACE' or
#'   'VOXELS'), 'vertex_index' (integer, 0-based index of the vertex in the
#'   surface for surface models, `NA` for volume models) and 'i', 'j', 'k'
#'   (integer, 0-based voxel indices for volume models, `NA` for surface models).
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' grayordinates <- cifti.grayordinates(cii, dim = 1L)
#' head(grayordinates)
#'
#' @family cifti functions
#' @export
cifti.grayordinates <- function(cii, dim = 0L) {
  cifti.check.object(cii)
  map <- cifti.map.for.dim(cii, dim)
  if (map$type != "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type '%s', not 'CIFTI_INDEX_TYPE_BRAIN_MODELS', so there are no brainordinates. Use cifti.parcels() for parcellated data.\n",
                 as.integer(dim), cii$filepath, map$type))
  }
  if (is.null(map$brain_models)) {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type 'CIFTI_INDEX_TYPE_BRAIN_MODELS' but contains no brain model entries.\n",
                 as.integer(dim), cii$filepath))
  }

  tables <- lapply(map$brain_models, function(model) {
    return(cifti.grayordinates.for.model(map, model))
  })
  result <- do.call(rbind, tables)
  rownames(result) <- NULL
  return(result)
}


#' @title Expand one brain model entry into a brainordinate table.
#'
#' @param map the indices map the brain model belongs to, see
#'   \code{\link{cifti.map.for.dim}}.
#'
#' @param model one brain model entry of the map.
#'
#' @return a data.frame, see \code{\link{cifti.grayordinates}}.
#'
#' @keywords internal
cifti.grayordinates.for.model <- function(map, model) {
  index <- seq.int(model$index_offset, length.out = model$index_count)
  n_entries <- length(index)
  vertex_index <- rep(NA_integer_, n_entries)
  voxel_i <- rep(NA_integer_, n_entries)
  voxel_j <- rep(NA_integer_, n_entries)
  voxel_k <- rep(NA_integer_, n_entries)

  if (model$model_type == "CIFTI_MODEL_TYPE_SURFACE") {
    if (is.null(model$vertex_indices)) {
      # No vertex indices means that the model covers all vertices of the surface,
      # in order (the header reader has verified that the counts match).
      vertex_index <- seq.int(0L, length.out = n_entries)
    } else {
      vertex_index <- as.integer(model$vertex_indices)
    }
  } else {
    if (is.null(model$voxel_indices_ijk)) {
      # No voxel indices means that the model covers all voxels of the volume. The
      # order is the raster order of the volume, i.e. the first voxel index varies
      # fastest, like in a NIFTI file.
      volume_dims <- map$volumes[[1L]]$dimensions
      all_voxels <- expand.grid(i = seq.int(0L, volume_dims[1L] - 1L),
                                j = seq.int(0L, volume_dims[2L] - 1L),
                                k = seq.int(0L, volume_dims[3L] - 1L))
      voxel_i <- as.integer(all_voxels$i)
      voxel_j <- as.integer(all_voxels$j)
      voxel_k <- as.integer(all_voxels$k)
    } else {
      voxel_i <- as.integer(model$voxel_indices_ijk[, 1L])
      voxel_j <- as.integer(model$voxel_indices_ijk[, 2L])
      voxel_k <- as.integer(model$voxel_indices_ijk[, 3L])
    }
  }

  return(data.frame(
    index = as.integer(index),
    structure = rep(model$brain_structure, n_entries),
    structure_short = rep(cifti.structure.short(model$brain_structure), n_entries),
    model_type = rep(sub("^CIFTI_MODEL_TYPE_", "", model$model_type), n_entries),
    vertex_index = as.integer(vertex_index),
    i = voxel_i,
    j = voxel_j,
    k = voxel_k,
    stringsAsFactors = FALSE
  ))
}


#' @title Get axis labels for a matrix dimension of a CIFTI-2 file.
#'
#' @description Get a label for each index of one matrix dimension of a CIFTI-2
#'   file, e.g. the map names of a `.dscalar`, the parcel names of a `.pconn` or
#'   identifiers for the grayordinates of a dense file. This is what
#'   \code{\link{read.cifti}} uses to name the dimensions of the data array.
#'
#' @inheritParams cifti.structures
#'
#' @return character vector of length `cii$matrix$dim_sizes[dim + 1]`, the labels,
#'   or `NULL` if no labels are known for this kind of dimension.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.ptseries.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cifti.dim.labels(cii, dim = 1L)
#'
#' @family cifti functions
#' @export
cifti.dim.labels <- function(cii, dim = 0L) {
  cifti.check.object(cii)
  map <- cifti.map.for.dim(cii, dim)
  dim_size <- as.integer(cii$matrix$dim_sizes[dim + 1L])

  if (map$type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    brainordinates <- cifti.grayordinates(cii, dim)
    labels <- ifelse(brainordinates$model_type == "SURFACE",
                     sprintf("%s_%d", brainordinates$structure_short, brainordinates$vertex_index),
                     sprintf("%s_%d_%d_%d", brainordinates$structure_short,
                             brainordinates$i, brainordinates$j, brainordinates$k))
    return(labels)
  }

  if (map$type == "CIFTI_INDEX_TYPE_PARCELS") {
    parcels <- cifti.parcels(cii, dim)
    return(as.character(parcels$name))
  }

  if (map$type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
    if (is.null(map$named_maps) || length(map$named_maps) != dim_size) {
      return(sprintf("map_%d", seq.int(0L, length.out = dim_size)))
    }
    names <- vapply(map$named_maps, function(named_map) as.character(named_map$name), character(1L))
    unnamed <- is.na(names) | !nzchar(names)
    names[unnamed] <- sprintf("map_%d", seq.int(0L, length.out = dim_size)[unnamed])
    return(names)
  }

  if (map$type == "CIFTI_INDEX_TYPE_SERIES") {
    if (is.null(map$series)) {
      return(sprintf("series_%d", seq.int(0L, length.out = dim_size))) # nocov
    }
    return(as.character((map$series$start + seq.int(0L, length.out = dim_size) * map$series$step) * 10^map$series$exponent))
  }

  return(NULL) # nocov
}


# --- Internal helpers (not exported) -----------------------------------------

#' @title Accept the input forms of a CIFTI-2 data object.
#'
#' @param x an `fs.cifti.data` object, an `fs.cifti` metadata object or the path
#'   of a CIFTI-2 file.
#'
#' @return an `fs.cifti.data` object, see \code{\link{read.cifti}}.
#'
#' @keywords internal
cifti.data.object <- function(x) {
  if (inherits(x, "fs.cifti.data")) {
    return(x)
  }
  if (inherits(x, "fs.cifti")) {
    return(read.cifti(x$filepath))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(read.cifti(x))
  }
  stop("Parameter 'x' must be an fs.cifti.data object (see read.cifti()), an fs.cifti object (see read.cifti.header()) or the path of a CIFTI-2 file.")
}


#' @title Get the brainordinate dimension of a CIFTI-2 file.
#'
#' @description Find the matrix dimension of a CIFTI-2 file that holds the brain
#'   structures. Most files have exactly one (dimension 1 for a `.dscalar`,
#'   `.dtseries` or `.dlabel`, dimension 0 for the special case of a
#'   `.dpconn`-style file), while connectome files like a `.dconn` have brain
#'   models in both dimensions.
#'
#' @inheritParams cifti.structures
#'
#' @return integer, the dimension. Stops with a descriptive error if there is no
#'   brainordinate dimension or if there are two.
#'
#' @keywords internal
cifti.brainordinate.dim <- function(cii) {
  dims <- cifti.brainordinate.dims(cii)
  if (length(dims) == 1L) {
    return(as.integer(dims))
  }
  if (length(dims) == 0L) {
    stop(sprintf("CIFTI-2 file '%s' has no brain model dimension, so it does not describe brainordinates. Use cifti.parcels() to get the parcels of a parcellated file.\n",
                 cii$filepath))
  }
  stop(sprintf(paste0("CIFTI-2 file '%s' has brain models in both matrix dimensions (the mappings cover dimensions %s), ",
                      "so the structures cannot be selected automatically. Please specify the parameter 'dim' (e.g. dim = 1).\n"),
               cii$filepath, paste(dims, collapse = " and ")))
}


#' @title Get all matrix dimensions of a CIFTI-2 file that hold brain models.
#'
#' @description A single `MatrixIndicesMap` element can apply to both matrix
#'   dimensions (this is the case for connectome files like `.dconn` and
#'   `.pconn`), so the dimensions have to be counted, not the mapping elements.
#'
#' @inheritParams cifti.structures
#'
#' @return integer vector, the dimensions that contain brain models (usually one,
#'   two for a file like `.dconn`).
#'
#' @keywords internal
cifti.brainordinate.dims <- function(cii) {
  dims <- integer(0L)
  for (map in cii$matrix$indices_maps) {
    if (identical(map$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")) {
      dims <- c(dims, as.integer(map$dims))
    }
  }
  return(unique(dims))
}


#' @title Get the number of vertices of one surface of a CIFTI-2 mapping.
#'
#' @description Get the size of the complete surface a brain model refers to. The
#'   `Surface` elements of the mapping are used if the file has them, otherwise
#'   the `SurfaceNumberOfVertices` attributes of the brain models are used: the
#'   `Surface` elements are optional, and the files written by Connectome
#'   Workbench (including the official CIFTI-2 example files) do not contain any,
#'   they report the surface size in the brain models only.
#'
#' @param map the indices map, see \code{\link{cifti.map.for.dim}}.
#'
#' @param structure character string, the canonical name of the brain structure.
#'
#' @return integer, the number of vertices, or `NA_integer_` if the structure has
#'   no surface model in this mapping.
#'
#' @keywords internal
cifti.surface.vertex.count <- function(map, structure) {
  counts <- cifti.surface.vertex.counts(map)
  if (structure %in% names(counts)) {
    return(as.integer(counts[[structure]]))
  }
  model_counts <- vapply(map$brain_models, function(model) {
    if (!identical(model$model_type, "CIFTI_MODEL_TYPE_SURFACE")) {
      return(NA_integer_)
    }
    if (!identical(cifti.structure.canonical(model$brain_structure), structure)) {
      return(NA_integer_)
    }
    return(as.integer(model$surface_number_of_vertices))
  }, integer(1L))
  model_counts <- model_counts[!is.na(model_counts)]
  if (length(model_counts) == 0L) {
    return(NA_integer_)
  }
  if (length(unique(model_counts)) > 1L) {
    stop(sprintf(paste0("The brain models for structure '%s' report %d different surface sizes (%s), so the data cannot be ",
                        "reconstructed. This makes the file invalid."),
                 as.character(structure), length(unique(model_counts)),
                 paste(sort(unique(model_counts)), collapse = ", ")))
  }
  return(model_counts[[1L]])
}


#' @title Read the data matrix of a CIFTI-2 file.
#'
#' @inheritParams read.cifti
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @return the data array, see \code{\link{read.cifti}}.
#'
#' @keywords internal
cifti.read.matrix <- function(cii, rows = NULL, columns = NULL) {
  dim_sizes <- as.integer(cii$matrix$dim_sizes)
  if (length(dim_sizes) < 1L || any(is.na(dim_sizes))) {
    stop(sprintf("The CIFTI-2 file '%s' has no data matrix dimensions.\n", cii$filepath)) # nocov
  }
  rows <- cifti.check.index.selection(rows, dim_sizes[1L], "rows", cii$filepath)
  if (length(dim_sizes) >= 2L) {
    columns <- cifti.check.index.selection(columns, dim_sizes[2L], "columns", cii$filepath)
  }

  bytes_per_value <- as.numeric(cii$niiheader$bitpix) / 8.

  if (is.null(columns)) {
    # Read the complete matrix in one go.
    cifti.validate.read.size(dim_sizes, bytes_per_value, cii$filepath)
    values <- cifti.read.values(cii, num_values = prod(dim_sizes), skip_values = 0L)
    data <- array(values, dim = dim_sizes)
    if (!is.null(rows)) {
      data <- cifti.subset.dim(data, 1L, rows)
    }
  } else {
    # Read only the selected columns: one column of a CIFTI-2 matrix is a
    # contiguous block of the file, so this needs one seek per column and never
    # touches the rest of the data.
    cifti.validate.read.size(c(dim_sizes[1L], length(columns)), bytes_per_value, cii$filepath)
    fh <- file(cii$filepath, "rb")
    on.exit(close(fh), add = TRUE)
    column_values <- lapply(columns, function(column) {
      offset <- as.numeric(cii$niiheader$vox_offset) + (column - 1) * dim_sizes[1L] * bytes_per_value
      seek(fh, where = offset, origin = "start", rw = "read")
      return(cifti.read.values(cii, num_values = dim_sizes[1L], fh = fh))
    })
    data <- array(unlist(column_values), dim = c(dim_sizes[1L], length(columns)))
    data <- cifti.subset.dim(data, 1L, rows)
  }

  # Name the array dimensions after the axis they belong to, and apply the
  # selection to the labels as well.
  dim_names <- lapply(seq_along(dim_sizes) - 1L, function(dim) {
    return(cifti.dim.labels(cii, dim))
  })
  if (!is.null(dim_names[[1L]]) && !is.null(rows)) {
    dim_names[[1L]] <- dim_names[[1L]][rows]
  }
  if (length(dim_names) >= 2L && !is.null(dim_names[[2L]]) && !is.null(columns)) {
    dim_names[[2L]] <- dim_names[[2L]][columns]
  }
  dimnames(data) <- dim_names

  return(data)
}


#' @title Read raw values from the data section of a CIFTI-2 file.
#'
#' @inheritParams cifti.read.matrix
#'
#' @param num_values integer, the number of values to read.
#'
#' @param skip_values integer, the number of values to skip first.
#'
#' @param fh optional connection to the file, positioned at the start of the data
#'   section if `skip_values` is 0. Will be opened (and closed) if left at `NULL`.
#'
#' @return vector of values, see \code{\link{read.nifti.values}}.
#'
#' @keywords internal
cifti.read.values <- function(cii, num_values, skip_values = 0L, fh = NULL) {
  own_connection <- is.null(fh)
  if (own_connection) {
    fh <- file(cii$filepath, "rb")
    on.exit(close(fh), add = TRUE)
    seek(fh, where = as.numeric(cii$niiheader$vox_offset) + skip_values * as.numeric(cii$niiheader$bitpix) / 8.,
         origin = "start", rw = "read")
  }
  return(read.nifti.values(fh, cii$niiheader$datatype, cii$niiheader$bitpix, as.numeric(num_values), cii$niiheader$endian))
}


#' @title Check the safety limit for reading a CIFTI-2 data matrix.
#'
#' @description Check the requested allocation against the package safety limit
#'   (see \code{\link{validate_allocation_size}}) and add a CIFTI specific hint to
#'   the error message if it is exceeded: the data of a large file can be read in
#'   parts by selecting matrix columns, which is not possible in the same way for
#'   other image formats.
#'
#' @param dims integer vector, the dimensions of the requested data matrix.
#'
#' @param bytes_per_elem numeric, the number of bytes per data value.
#'
#' @param filepath character string, the path of the file, used in the error message.
#'
#' @return `NULL`, invisibly. Stops if the limit is exceeded.
#'
#' @keywords internal
cifti.validate.read.size <- function(dims, bytes_per_elem, filepath) {
  tryCatch(
    validate_allocation_size(dims, bytes_per_elem,
                             label = sprintf("the data matrix of CIFTI-2 file '%s'", filepath)),
    error = function(e) {
      stop(paste0(conditionMessage(e),
                  "For a CIFTI-2 file you can read only the part you need: the indices of matrix\n",
                  "  dimension 1 (the grayordinates of a dense file) can be requested with the\n",
                  "  'columns' parameter of read.cifti(), and this does not read the rest of the\n",
                  "  file. See ?read.cifti.\n"))
    }
  )
  return(invisible(NULL))
}


#' @title Check a matrix index selection.
#'
#' @param selection integer vector or `NULL`, the indices to check.
#'
#' @param max_index integer, the size of the matrix dimension.
#'
#' @param what character string, the name of the selection ('rows' or 'columns'),
#'   used in the error message.
#'
#' @param filepath character string, the path of the file, used in the error message.
#'
#' @return the selection as an integer vector, or `NULL`.
#'
#' @keywords internal
cifti.check.index.selection <- function(selection, max_index, what, filepath) {
  if (is.null(selection)) {
    return(NULL)
  }
  if (!is.numeric(selection) || length(selection) == 0L || any(is.na(selection)) ||
      any(selection < 1L) || any(selection > max_index) || any(selection != as.integer(selection))) {
    stop(sprintf("Parameter '%s' must be an integer vector with values in range 1 to %d for CIFTI-2 file '%s' (the indices are 1-based, like everywhere in R).\n",
                 what, max_index, filepath))
  }
  return(as.integer(selection))
}


#' @title Subset one dimension of an array.
#'
#' @param data the array to subset.
#'
#' @param dim integer, the dimension to subset.
#'
#' @param selection integer vector or `NULL`, the indices to keep.
#'
#' @return the subset, with the dimensions of `data` preserved.
#'
#' @keywords internal
cifti.subset.dim <- function(data, dim, selection) {
  if (is.null(selection)) {
    return(data)
  }
  index <- rep(list(TRUE), length(dim(data)))
  index[[dim]] <- selection
  return(do.call(`[`, c(list(data), index, list(drop = FALSE))))
}


#' @title Move one dimension of an array to the front.
#'
#' @param data the array.
#'
#' @param dim integer, the dimension to move, counted from 0 (CIFTI style).
#'
#' @return the array with the requested dimension first, and all other dimensions
#'   in their original order.
#'
#' @keywords internal
cifti.dim.to.front <- function(data, dim) {
  if (dim == 0L) {
    return(data)
  }
  return(aperm(data, c(dim + 1L, seq_len(dim), if (dim + 1L < length(dim(data))) seq.int(dim + 2L, length(dim(data))))))
}


#' @title Create an array like the input, with a different first dimension.
#'
#' @param data the array, its first dimension is the one that is replaced.
#'
#' @param first_dim_size integer, the size of the new first dimension.
#'
#' @return an array of `NA` of the data type of `data`, with the new first
#'   dimension and all other dimensions of `data`.
#'
#' @keywords internal
cifti.array.with.new.first.dim <- function(data, first_dim_size) {
  na_value <- if (is.integer(data)) NA_integer_ else if (is.logical(data)) NA else NA_real_
  return(array(na_value, dim = c(as.integer(first_dim_size), dim(data)[-1L])))
}


#' @title Assign rows of an array, reordered or repeated.
#'
#' @param target array, the array to write to.
#'
#' @param to integer vector, the row indices of `target` to write.
#'
#' @param from integer vector, the row indices of `data` to read.
#'
#' @param data array, the source array.
#'
#' @return `target` with the requested rows assigned.
#'
#' @keywords internal
cifti.assign.rows <- function(target, to, from, data) {
  target_index <- c(list(to), rep(list(TRUE), length(dim(target)) - 1L))
  source_index <- c(list(from), rep(list(TRUE), length(dim(data)) - 1L))
  values <- do.call(`[`, c(list(data), source_index, list(drop = FALSE)))
  return(do.call(`[<-`, c(list(target), target_index, list(values))))
}
