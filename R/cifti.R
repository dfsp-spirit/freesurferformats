# Functions for working with CIFTI files.
#
# Reading is native: the XML metadata is parsed by the functions in R/read_cifti_header.R,
# the data values are read by the functions in R/read_cifti.R. Files in CIFTI-1 format and
# gzipped CIFTI files are not supported, see the reader for the details.
#
# General CIFTI information: https://www.nitrc.org/projects/cifti/
# Specs:
#  - CIFTI v1 spec: https://www.nitrc.org/plugins/mwiki/index.php/cifti:Cifti-1
#  - CITFI v2 full spec (PDF): https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955
# Related:
#  - CIFTI v2 changes from v1: https://www.nitrc.org/forum/forum.php?thread_id=4381&forum_id=1955
#  - Great article on NIFTI v2 format by A Winkler: https://brainder.org/2015/04/03/the-nifti-2-file-format/


#' @title Read surface morphometry data from CIFTI dscalar files.
#'
#' @description Read the data from a CIFTI dscalar file (or any other CIFTI-2 file with a
#'   single brainordinate dimension, e.g. a '.dtseries') and reconstruct it for one brain
#'   structure, based on the metadata like vertex counts, indices and offset in the CIFTI
#'   file. The result is a vector with one value per vertex of the surface.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dscalar.nii'. Note that this is NOT a NIFTI file, despite the '.nii' part. It uses a CIFTIv2 header though. See the spec for details. An 'fs.cifti' object from \code{\link{read.cifti.header}} or an 'fs.cifti.data' object from \code{\link{read.cifti}} may be given instead of a path (this is faster if you need the data of several structures). An object created by the 'cifti' package is still accepted for backwards compatibility.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', the concatenated data for 'lh' (first) and 'rh' will be returned, but you will get no information on hemi boundaries. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @param data_column integer, the data column to return. A CIFTI file can contain several measures in different data columns (e.g., cortical thickness and surface area) in a single file. This specifies which column/measure you want. The columns are not named, so you will need to know this in advance if the file has several measures.
#'
#' @return The reconstructed data for the given surface, one value per vertex in the surface. The value for vertices which did not have a value in the CIFTI data is set to `NA`. Note that the result always has the length of the *complete* surface: CIFTI files like the ones used in the HCP leave out the medial wall vertices, and the result reports those as `NA`.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' morph_lh <- read.fs.morph.cifti(cifti_file, "lh")
#' length(morph_lh)
#' morph_lh2 <- read.fs.morph.cifti(cifti_file, "lh", 2L)
#' morph_both <- read.fs.morph.cifti(cifti_file, "both")
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.morph.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT", data_column = 1L) {
  if (identical(brain_structure, "both")) {
    return(c(
      read.fs.morph.cifti(filepath, brain_structure = "lh", data_column = data_column),
      read.fs.morph.cifti(filepath, brain_structure = "rh", data_column = data_column)
    ))
  }

  if (inherits(filepath, "cifti")) {
    return(.cifti.legacy.surface.matrix(.get.cifti(filepath),
                                        .normalize.cifti.brain.structure(brain_structure))[, data_column])
  }

  x <- cifti.data.object(filepath)
  structure <- cifti.structure.from.specifier(x$header, brain_structure)
  return(cifti.dense.structure.matrix(x, structure)[, data_column])
}


#' @title Read surface parcellation data from CIFTI dlabel files.
#'
#' @description Read the data from a CIFTI dlabel file (a dense surface parcellation, i.e., an integer label key per vertex) and return the per-vertex label keys for a single brain structure. This is the CIFTI analogue of a FreeSurfer annotation file.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dlabel.nii'. Note that this is NOT a NIfTI file, despite the '.nii' part; it uses a CIFTI 2 header instead. See the spec for details. An 'fs.cifti' object from \code{\link{read.cifti.header}} or an 'fs.cifti.data' object from \code{\link{read.cifti}} may be given instead of a path (this is faster if you need the data of several structures). An object created by the 'cifti' package is still accepted for backwards compatibility.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', the concatenated data for 'lh' (first) and 'rh' will be returned, but you will get no information on hemi boundaries. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @param data_column integer, the data column (map) to return. A CIFTI dlabel file can contain several parcellations in different columns in a single file; this selects which one. The columns are not named, so you will need to know this in advance if the file has several maps (you can use the 'with_label_table' option to inspect the map names).
#'
#' @param with_label_table logical, whether to also return the label table (the mapping from label key to region name and RGBA color) for the selected map. If 'TRUE', the return value is a list, see the Value section.
#'
#' @return If 'with_label_table' is 'FALSE' (the default), an integer vector with one label key per vertex in the surface. Vertices which did not have a value in the CIFTI data are set to `NA`; a key of 0 denotes the unknown / medial wall label (see the label table). If 'with_label_table' is 'TRUE', a named list with entries: 'label_keys' (the integer vector), 'label_table' (a data.frame with columns 'Key', 'Red', 'Green', 'Blue', 'Alpha' and 'Label', where the RGBA values are in the range 0 to 1), 'map_name' (the name of the selected map as a character string, or 'NULL' if unnamed), and 'brain_structure' (a character string). If 'brain_structure' is 'both' and 'with_label_table' is 'TRUE', a named list with entries 'lh' and 'rh', each as described above.
#'
#' @examples
#' label_file <- system.file("extdata", "cifti", "tiny.dlabel.nii", package = "freesurferformats")
#' parc_lh <- read.fs.parcellation.cifti(label_file, "lh")
#' table(parc_lh)
#' parc_with_table <- read.fs.parcellation.cifti(label_file, "lh", with_label_table = TRUE)
#' parc_with_table$label_table
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.parcellation.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT", data_column = 1L, with_label_table = FALSE) {
  if (identical(brain_structure, "both")) {
    if (isTRUE(with_label_table)) {
      return(list(lh = read.fs.parcellation.cifti(filepath, "lh", data_column = data_column, with_label_table = TRUE),
                  rh = read.fs.parcellation.cifti(filepath, "rh", data_column = data_column, with_label_table = TRUE)))
    }
    return(c(read.fs.parcellation.cifti(filepath, "lh", data_column = data_column, with_label_table = FALSE),
             read.fs.parcellation.cifti(filepath, "rh", data_column = data_column, with_label_table = FALSE)))
  }

  if (inherits(filepath, "cifti")) {
    cii <- .get.cifti(filepath)
    brain_structure <- .normalize.cifti.brain.structure(brain_structure)
    label_keys <- as.integer(.cifti.legacy.surface.matrix(cii, brain_structure)[, data_column])
    if (!isTRUE(with_label_table)) {
      return(label_keys)
    }
    return(list(label_keys = label_keys,
                label_table = .cifti.legacy.label.table(cii, data_column),
                map_name = .cifti.legacy.map.name(cii, data_column),
                brain_structure = brain_structure))
  }

  x <- cifti.data.object(filepath)
  structure <- cifti.structure.from.specifier(x$header, brain_structure)
  label_keys <- as.integer(cifti.dense.structure.matrix(x, structure)[, data_column])

  if (!isTRUE(with_label_table)) {
    return(label_keys)
  }

  label_dim <- cifti.other.dim(x$header, cifti.structure.data.dim(x$header, structure))
  labels <- cifti.label.table(x$header, dim = label_dim, map = data_column)
  if (is.null(labels)) {
    stop(sprintf("No label table found in CIFTI-2 file '%s' for data column %d.\n", x$header$filepath, as.integer(data_column)))
  }
  return(list(label_keys = label_keys,
              label_table = .cifti.label.table.for.users(labels),
              map_name = cifti.map.name(x$header, dim = label_dim, map = data_column),
              brain_structure = structure))
}


#' @title Read surface time series data from CIFTI dtseries files.
#'
#' @description Read the data from a CIFTI dtseries file (a dense surface time series) and return the per-vertex time series for a single brain structure as a matrix.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dtseries.nii'. Note that this is NOT a NIfTI file, despite the '.nii' part; it uses a CIFTI 2 header instead. See the spec for details. An 'fs.cifti' object from \code{\link{read.cifti.header}} or an 'fs.cifti.data' object from \code{\link{read.cifti}} may be given instead of a path (this is faster if you need the data of several structures). An object created by the 'cifti' package is still accepted for backwards compatibility.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', a named list with entries 'lh' and 'rh' will be returned. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @return A numeric matrix with one row per vertex in the surface and one column per time point (series point) in the file. The value for vertices which did not have a value in the CIFTI data is set to `NA`. If 'brain_structure' is 'both', a named list with entries 'lh' and 'rh', each a matrix as described above.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dtseries.nii", package = "freesurferformats")
#' series_lh <- read.fs.series.cifti(cifti_file, "lh")
#' dim(series_lh)
#' series_both <- read.fs.series.cifti(cifti_file, "both")
#' names(series_both)
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.series.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT") {
  if (identical(brain_structure, "both")) {
    return(list(lh = read.fs.series.cifti(filepath, "lh"),
                rh = read.fs.series.cifti(filepath, "rh")))
  }

  if (inherits(filepath, "cifti")) {
    return(.cifti.legacy.surface.matrix(.get.cifti(filepath),
                                        .normalize.cifti.brain.structure(brain_structure)))
  }

  x <- cifti.data.object(filepath)
  structure <- cifti.structure.from.specifier(x$header, brain_structure)
  return(cifti.dense.structure.matrix(x, structure))
}


# --- Internal helpers (not exported) -----------------------------------------

#' @title Get the dense data matrix of one structure for the user-facing readers.
#'
#' @description Workhorse of \code{\link{read.fs.morph.cifti}} and friends: like
#'   \code{\link{cifti.structure.data}}, but requires the structure to have a
#'   surface part and returns the per-vertex matrix directly. This keeps the
#'   user-facing functions working for the structures they support today, and
#'   turns the unsupported ones into a descriptive error.
#'
#' @param x an `fs.cifti.data` object, a file path or an `fs.cifti` object, see
#'   \code{\link{cifti.structure.data}}.
#'
#' @param structure character string, the canonical name of the brain structure.
#'
#' @return a numeric matrix, one row per surface vertex, one column per index of
#'   the other matrix dimension.
#'
#' @keywords internal
cifti.dense.structure.matrix <- function(x, structure) {
  x <- cifti.data.object(x)
  data <- cifti.structure.data(x, structure)
  if (is.null(data$surface)) {
    stop(sprintf(paste0("The brain structure '%s' has no surface part in CIFTI-2 file '%s' (it is a volume model, see the 'volume' entry of ",
                        "cifti.structure.data()). Data for volume structures cannot be returned as per-vertex data, because the voxels of a ",
                        "structure are not a rectangular block of a volume.\n"),
                 as.character(structure), x$header$filepath))
  }
  return(data$surface)
}


#' @title Resolve a user-supplied structure specifier to a canonical name.
#'
#' @description Accept the structure spellings the user-facing readers support: a
#'   structure name in any of the supported spellings, or the index of a structure
#'   in the file (deprecated, but part of the documented interface of the
#'   readers).
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param brain_structure character string or integer, the specifier.
#'
#' @return character string, the canonical structure name.
#'
#' @keywords internal
cifti.structure.from.specifier <- function(cii, brain_structure) {
  if (is.numeric(brain_structure)) {
    if (length(brain_structure) != 1L || is.na(brain_structure) || brain_structure < 1L) {
      stop("If 'brain_structure' is given as an index, it must be a single positive integer.")
    }
    dim <- cifti.brainordinate.dim(cii)
    structures <- unique(cifti.grayordinates(cii, dim)$structure)
    if (brain_structure > length(structures)) {
      stop(sprintf("CIFTI-2 file '%s' has %d brain structures in matrix dimension %d, so there is no structure with index %d.\n",
                   cii$filepath, length(structures), as.integer(dim), as.integer(brain_structure)))
    }
    return(structures[[as.integer(brain_structure)]])
  }
  return(cifti.structure.canonical(brain_structure))
}


#' @title Get the matrix dimension a brain structure lives in.
#'
#' @description The counterpart of \code{\link{cifti.other.dim}} for a structure
#'   name: look up the dimension of the file that contains brain models for the
#'   given structure. Needed by the user-facing readers, which get a structure name
#'   from the user and must decide which dimension of the data matrix it refers to.
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param structure character string, the canonical name of the brain structure.
#'
#' @return integer, the matrix dimension.
#'
#' @keywords internal
cifti.structure.data.dim <- function(cii, structure) {
  dims <- integer(0L)
  for (map in cii$matrix$indices_maps) {
    if (!identical(map$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS") || is.null(map$brain_models)) {
      next
    }
    found <- vapply(map$brain_models, function(model) {
      return(identical(cifti.structure.canonical(model$brain_structure), structure))
    }, logical(1L))
    if (any(found)) {
      dims <- c(dims, as.integer(map$dims))
    }
  }
  if (length(unique(dims)) != 1L) {
    # cifti.brainordinate.dim() produces the good error message for the other cases.
    return(cifti.brainordinate.dim(cii))
  }
  return(as.integer(unique(dims)))
}


#' @title Get the other matrix dimension of a 2-dimensional CIFTI-2 matrix.
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param dim integer, the matrix dimension to exclude.
#'
#' @return integer, the other matrix dimension. Stops for files with more than two
#'   matrix dimensions.
#'
#' @keywords internal
cifti.other.dim <- function(cii, dim) {
  if (length(cii$matrix$dim_sizes) != 2L) {
    stop(sprintf("CIFTI-2 file '%s' has %d matrix dimensions, this function supports only files with 2 dimensions.\n",
                 cii$filepath, length(cii$matrix$dim_sizes)))
  }
  return(as.integer(setdiff(0:1, as.integer(dim))))
}


#' @title Get the name of one named map of a CIFTI-2 file.
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param dim integer, the matrix dimension that holds the named maps.
#'
#' @param map integer, the number of the map (1-based).
#'
#' @return character string, the map name, or `NULL` if the map is unnamed.
#'
#' @keywords internal
cifti.map.name <- function(cii, dim, map = 1L) {
  named_maps <- cifti.map.for.dim(cii, dim)$named_maps
  if (is.null(named_maps) || length(named_maps) < map) {
    return(NULL)
  }
  name <- as.character(named_maps[[as.integer(map)]]$name)
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    return(NULL)
  }
  return(name)
}


#' @title Convert a label table to the format the package has always returned.
#'
#' @description The readers document a label table with the column names 'Key',
#'   'Red', 'Green', 'Blue', 'Alpha' and 'Label'; keep that spelling (and the
#'   column order) for the user-facing functions, while the internal representation
#'   uses lower case column names and also stores the optional label coordinates.
#'
#' @param labels a data.frame as returned by \code{\link{cifti.label.table}}.
#'
#' @return a data.frame with the columns 'Key', 'Red', 'Green', 'Blue', 'Alpha'
#'   and 'Label'.
#'
#' @keywords internal
.cifti.label.table.for.users <- function(labels) {
  return(data.frame(
    Key = as.integer(labels$key),
    Red = labels$red,
    Green = labels$green,
    Blue = labels$blue,
    Alpha = labels$alpha,
    Label = labels$label,
    stringsAsFactors = FALSE
  ))
}


# --- Support for objects created by the 'cifti' package ----------------------
#
# The user-facing readers used to work on objects created by the 'cifti' package (that
# was the documented workaround for bug #9 of that package, see
# https://github.com/muschellij2/cifti/issues/9). Reading is native now, but passing
# such an object keeps working, so that existing client code does not break. The helpers
# below are only used for that case.

# Return a parsed CIFTI object, reading the file if needed, and require the 'cifti' package.
.get.cifti <- function(filepath) {
  if (!requireNamespace("cifti", quietly = TRUE)) {
    stop("Reading files in CIFTI format requires the 'cifti' package to be installed.") # nocov
  }
  if (cifti::is.cifti(filepath)) {
    return(filepath)
  }
  return(cifti::read_cifti(filepath))
}

# Map the user-facing 'lh' / 'rh' aliases to canonical CIFTI brain structure names.
.normalize.cifti.brain.structure <- function(brain_structure) {
  if (is.character(brain_structure)) {
    if (identical(brain_structure, "lh")) {
      return("CIFTI_STRUCTURE_CORTEX_LEFT")
    }
    if (identical(brain_structure, "rh")) {
      return("CIFTI_STRUCTURE_CORTEX_RIGHT")
    }
  }
  return(brain_structure)
}

# Resolve a brain structure specifier (name or index) to an index into cii$BrainModel.
.cifti.brain.structure.index <- function(cii, brain_structure) {
  if (is.numeric(brain_structure)) {
    return(as.integer(brain_structure))
  }
  brain_struct_names <- cifti::cifti_brain_structs(cii)
  if (!brain_structure %in% brain_struct_names) {
    stop(sprintf("No brain structure named '%s' in the %d CIFTI brain structures '%s'.\n",
                 brain_structure, length(brain_struct_names),
                 paste(brain_struct_names, collapse = ", ")))
  }
  return(which(brain_struct_names == brain_structure))
}

# Extract the full per-vertex data matrix for one surface brain structure from an object
# created by the 'cifti' package.
# Returns a (SurfaceNumberOfVertices x ncol(cii$data)) numeric matrix, with NA for
# vertices that have no value in the file. Errors for non-surface brain structures.
.cifti.legacy.surface.matrix <- function(cii, brain_structure) {
  brain_struct_index <- .cifti.brain.structure.index(cii, brain_structure)
  brain_struct_type <- attr(cii$BrainModel[[brain_struct_index]], "ModelType") # should be "CIFTI_MODEL_TYPE_SURFACE"

  if (is.null(brain_struct_type)) {
    # It's a volume, and the model is just an nx3 integer matrix of n (i,j,k) voxel indices.
    # We still have to confirm this in the spec though, currently this is based on the (official) CIFTI2 example files only.
    stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type NULL (most likely a volume part).\n", as.character(brain_structure)))
  }
  if (brain_struct_type != "CIFTI_MODEL_TYPE_SURFACE") {
    stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type '%s'.\n", as.character(brain_structure), brain_struct_type))
  }

  md_index_count <- attr(cii$BrainModel[[brain_struct_index]], "IndexCount")
  md_index_offset <- attr(cii$BrainModel[[brain_struct_index]], "IndexOffset") + 1L
  vert_indices <- cii$BrainModel[[brain_struct_index]] + 1L
  surface_num_verts <- attr(cii$BrainModel[[brain_struct_index]], "SurfaceNumberOfVertices")

  if (length(vert_indices) != md_index_count) {
    warning(sprintf("There are %d vertex indices given for the %d data values.\n",
                    length(vert_indices), md_index_count))
  }

  data_matrix <- as.matrix(cii$data)
  end_index <- md_index_offset + md_index_count - 1L
  result <- matrix(NA_real_, nrow = surface_num_verts, ncol = ncol(data_matrix))
  result[vert_indices, ] <- data_matrix[md_index_offset:end_index, , drop = FALSE]
  return(result)
}

# Return the label table (key -> name + RGBA) for one map of a 'cifti' package object.
.cifti.legacy.label.table <- function(cii, data_column = 1L) {
  lut <- cii$NamedMap$look_up_table
  if (is.null(lut)) {
    stop("No label table (NamedMap) found in this CIFTI file.")
  }
  n_lut <- length(lut)
  if (data_column < 1L || data_column > n_lut) {
    stop(sprintf("data_column %d is out of range: the file has %d maps (label tables).\n",
                 as.integer(data_column), n_lut))
  }
  return(lut[[data_column]])
}

# Return the name of one map of a 'cifti' package object, or NULL if it is unnamed or missing.
.cifti.legacy.map.name <- function(cii, data_column = 1L) {
  map_names <- cii$NamedMap$map_names
  if (!is.character(map_names) || length(map_names) < data_column) {
    return(NULL)
  }
  if (nzchar(map_names[data_column])) {
    return(map_names[data_column])
  }
  return(NULL)
}
