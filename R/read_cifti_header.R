# Functions for reading the metadata (XML) part of CIFTI version 2 files.
#
# A CIFTI-2 file is a NIFTI-2 file whose XML metadata is stored in a NIFTI v2
# header extension with the extension code 32 (see R/nifti2_extensions.R for the
# container and R/read_nifti2.R for the NIfTI-2 header). Everything that describes
# what the numbers in the file mean is in that XML: the matrix dimensions and
# their meaning (brain models, parcels, series, scalars or labels), the brain
# structures and vertex/voxel indices, the parcels, and the label tables.
#
# The format is defined by the CIFTI-2 specification, which is only available as a
# scanned PDF (<https://www.nitrc.org/forum/forum.php?thread_id=4380&forum_id=1955>),
# and implemented by Connectome Workbench and nibabel. This reader follows those
# two implementations.
#
# See https://www.nitrc.org/projects/cifti/ for an overview.


#' @title Read CIFTI-2 header (XML metadata).
#'
#' @description Read the metadata of a CIFTI version 2 file, i.e. the XML
#'   document that describes everything except the actual data values: the matrix
#'   dimensions and their meaning, the brain models (surface vertices and volume
#'   voxels), the parcels, the series information, and the label tables. CIFTI-2
#'   files are NIFTI-2 files that store this XML in a NIFTI v2 header extension
#'   with the extension code 32. Use \code{read.cifti} to read the data
#'   values as well (not implemented yet), or the accessor functions
#'   \code{\link{cifti.structures}}, \code{\link{cifti.parcels}},
#'   \code{\link{cifti.series.info}} and \code{\link{cifti.label.table}} to
#'   inspect the result.
#'
#' @param filepath character string, the path to a CIFTI-2 file (usually one of
#'   `.dscalar.nii`, `.dtseries.nii`, `.dlabel.nii`, `.dconn.nii`, `.pscalar.nii`,
#'   `.ptseries.nii`, `.pconn.nii`, `.dpconn.nii` or `.pdconn.nii`). Note that
#'   this is not a NIFTI file, despite the `.nii` part. Gzipped CIFTI files are
#'   not supported, because the CIFTI-2 format forbids compression.
#'
#' @return an `fs.cifti` object, a named list with the entries: 'filepath' (the
#'   file path), 'niiheader' (the NIFTI-2 header as returned by
#'   \code{\link{read.nifti2.header}}), 'version' (the CIFTI version, always '2'
#'   for files that can be read), and 'matrix', a named list with the entries
#'   'metadata' (the matrix-level metadata, a named list of character strings, in
#'   file order; the names may repeat) and 'indices_maps' (a list of the
#'   `MatrixIndicesMap` elements, see below), plus 'dim_sizes' (the integer sizes
#'   of the matrix dimensions; these are stored in entries 5, 6, ... of the `dim`
#'   field of the NIFTI-2 header, and R index vectors are 1-based, so the 6th
#'   entry of `dim` holds matrix dimension 0).
#'
#'   Each element of `indices_maps` is a named list with the entries: 'dims'
#'   (integer vector, the 0-based matrix dimensions this mapping applies to; it
#'   has several entries for files like a `.dconn`, where one mapping describes
#'   both dimensions), 'applies_to' (the same as a character string, as found in
#'   the file), 'type' (character string, one of 'CIFTI_INDEX_TYPE_BRAIN_MODELS',
#'   'CIFTI_INDEX_TYPE_PARCELS', 'CIFTI_INDEX_TYPE_SERIES',
#'   'CIFTI_INDEX_TYPE_SCALARS' or 'CIFTI_INDEX_TYPE_LABELS'), 'size' (integer
#'   vector, the size of the dimension(s) from 'dims'), 'series' (a list with
#'   entries 'number_of_series_points', 'start', 'step', 'exponent' and 'unit',
#'   for series mappings; `NULL` otherwise), 'surfaces' (a list of lists with
#'   entries 'brain_structure' and 'surface_number_of_vertices'), 'volumes' (a
#'   list of lists with entries 'dimensions' (integer vector of length 3),
#'   'meter_exponent' and 'transformation_matrix' (4x4 numeric matrix, row-major
#'   as in the file, mapping 0-based voxel indices to coordinates in units of
#'   `10^meter_exponent`)), 'brain_models' (a list of lists with entries
#'   'index_offset' (0-based), 'index_count', 'model_type',
#'   'brain_structure', 'surface_number_of_vertices' (surfaces only, `NA`
#'   otherwise), 'vertex_indices' (0-based integer vector, or `NULL` if all
#'   vertices of the surface are used) and 'voxel_indices_ijk' (an n x 3 integer
#'   matrix of 0-based voxel indices, or `NULL`) ), 'parcels' (a list of lists
#'   with entries 'index' (0-based position in the list), 'name', 'vertices'
#'   (named list of 0-based vertex index vectors, named by the canonical brain
#'   structure name, e.g. 'CORTEX_LEFT') and 'voxel_indices_ijk'), and
#'   'named_maps' (a list of lists with entries 'name', 'metadata' and 'labels';
#'   'labels' is a data.frame with the columns 'key', 'red', 'green', 'blue',
#'   'alpha', 'label', 'x', 'y' and 'z', see \code{\link{cifti.label.table}}).
#'
#' @note This function performs structural validation of the XML and stops with a
#'   descriptive error if the file is not a CIFTI-2 file, if the XML is
#'   inconsistent (e.g., index ranges that do not cover the data, or a number of
#'   parcels that does not match the matrix dimension), or if a file in the
#'   outdated CIFTI-1 format is passed (which has to be converted first, see the
#'   error message).
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cii
#' cii$matrix$indices_maps[[1]]$type
#' cifti.structures(cii, dim = 1L)
#'
#' @family cifti functions
#' @export
read.cifti.header <- function(filepath) {
  if (!is.character(filepath) || length(filepath) != 1L || is.na(filepath)) {
    stop("Parameter 'filepath' must be a character string.")
  }
  if (!file.exists(filepath)) {
    stop(sprintf("File '%s' does not exist.\n", filepath))
  }
  if (guess.filename.is.gzipped(filepath, gz_extensions = c(".gz"))) {
    stop(sprintf(paste0(
      "CIFTI-2 files must not be compressed, but the file name '%s' suggests a gzipped file. ",
      "The CIFTI-2 format forbids compression so that random access to the data remains possible. ",
      "Please decompress the file (e.g. with 'gunzip') before reading it.\n"), filepath))
  }

  niiheader <- tryCatch(
    read.nifti2.header(filepath),
    error = function(e) {
      if (cifti.file.looks.like.cifti1(filepath)) {
        stop(sprintf(paste0(
          "File '%s' looks like a CIFTI-1 file, which is not supported. ",
          "Convert it to CIFTI-2 first, e.g. with the Connectome Workbench command ",
          "'wb_command -cifti-convert -version-convert <input> 2 <output>'.\n"), filepath))
      }
      stop(sprintf("Failed to read '%s' as a NIFTI-2 file, so it cannot be a CIFTI-2 file: %s\n", filepath, conditionMessage(e)))
    }
  )

  extension <- nifti2.get.extension(niiheader, CIFTI_EXTENSION_CODE)
  if (is.null(extension)) {
    stop(sprintf(paste0(
      "File '%s' is not a CIFTI-2 file: it has no NIFTI v2 header extension with code %d, ",
      "which is the extension code that holds the CIFTI XML metadata.\n"), filepath, CIFTI_EXTENSION_CODE))
  }

  cii <- cifti.parse.xml(nifti2.extension.text(extension), filepath = filepath, niiheader = niiheader)
  return(cii)
}


#' @title The NIFTI v2 extension code that holds the CIFTI-2 XML metadata.
#'
#' @description The CIFTI-2 XML document is stored in a NIFTI v2 header extension
#'   with this code.
#'
#' @keywords internal
CIFTI_EXTENSION_CODE <- 32L


#' @title The CIFTI-2 index types (mapping types).
#'
#' @keywords internal
cifti.index.types <- function() {
  return(c(
    "CIFTI_INDEX_TYPE_BRAIN_MODELS",
    "CIFTI_INDEX_TYPE_PARCELS",
    "CIFTI_INDEX_TYPE_SERIES",
    "CIFTI_INDEX_TYPE_SCALARS",
    "CIFTI_INDEX_TYPE_LABELS"
  ))
}


#' @title The CIFTI-2 brain model types.
#'
#' @keywords internal
cifti.model.types <- function() {
  return(c("CIFTI_MODEL_TYPE_SURFACE", "CIFTI_MODEL_TYPE_VOXELS"))
}


#' @title Print an fs.cifti instance.
#'
#' @param x an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param ... ignored.
#'
#' @return the instance, invisibly.
#'
#' @family cifti functions
#' @export
print.fs.cifti <- function(x, ...) {
  cat(sprintf("CIFTI-2 file '%s' (NIFTI-2, %d data values per matrix entry).\n", x$filepath, x$niiheader$bitpix / 8L))
  if (!is.null(x$matrix$dim_sizes)) {
    cat(sprintf("Matrix dimensions: %s (CIFTI dimension %s).\n",
                paste(x$matrix$dim_sizes, collapse = " x "),
                paste(seq_along(x$matrix$dim_sizes) - 1L, collapse = ", ")))
  }
  for (map in x$matrix$indices_maps) {
    cat(sprintf("  Dimension %s: %s, %s per dimension.\n",
                paste(map$dims, collapse = ","),
                cifti.map.type.description(map$type),
                paste(map$size, collapse = ",")))
    if (!is.null(map$series)) {
      cat(sprintf("    %d series points, %s %s per index (start %s, exponent %d).\n",
                  map$series$number_of_series_points, format(map$series$step),
                  map$series$unit, format(map$series$start), map$series$exponent))
    }
    if (!is.null(map$brain_models)) {
      structures <- vapply(map$brain_models, function(bm) cifti.structure.short(bm$brain_structure), character(1L))
      cat(sprintf("    %d brain model entries: %s\n", length(map$brain_models), paste(structures, collapse = ", ")))
    }
    if (!is.null(map$parcels)) {
      cat(sprintf("    %d parcels, first: %s\n", length(map$parcels), paste(utils::head(vapply(map$parcels, function(p) p$name, character(1L)), 5L), collapse = ", ")))
    }
    if (!is.null(map$named_maps)) {
      cat(sprintf("    %d named maps.\n", length(map$named_maps)))
    }
  }
  return(invisible(x))
}


#' @title Describe a CIFTI-2 index type for humans.
#'
#' @param type character string, a CIFTI-2 index type, see \code{\link{cifti.index.types}}.
#'
#' @return character string, a short description.
#'
#' @keywords internal
cifti.map.type.description <- function(type) {
  descriptions <- c(
    "CIFTI_INDEX_TYPE_BRAIN_MODELS" = "brain models (surface vertices and/or volume voxels)",
    "CIFTI_INDEX_TYPE_PARCELS" = "parcels",
    "CIFTI_INDEX_TYPE_SERIES" = "a series (e.g. time points)",
    "CIFTI_INDEX_TYPE_SCALARS" = "scalars (maps)",
    "CIFTI_INDEX_TYPE_LABELS" = "labels (maps with label tables)"
  )
  if (type %in% names(descriptions)) {
    return(descriptions[[type]])
  }
  return(type) # nocov
}


#' @title Normalize a CIFTI brain structure name.
#'
#' @description CIFTI brain structure names are written in several spellings by
#'   different software: Connectome Workbench uses `CIFTI_STRUCTURE_CORTEX_LEFT`,
#'   nibabel also accepts `CortexLeft`, and this package uses the aliases `lh` and
#'   `rh` (and also `left`/`right`) for the cortical surfaces. This function
#'   normalizes all of them to the canonical `CIFTI_STRUCTURE_*` spelling.
#'
#' @param brain_structure character string, a brain structure name in any of the
#'   supported spellings.
#'
#' @return character string, the canonical structure name (e.g.
#'   'CIFTI_STRUCTURE_CORTEX_LEFT'), or `NA_character_` if the input is `NA`.
#'
#' @keywords internal
cifti.structure.canonical <- function(brain_structure) {
  if (length(brain_structure) != 1L) {
    stop("Parameter 'brain_structure' must be a single character string.") # nocov
  }
  if (is.na(brain_structure)) {
    return(NA_character_)
  }
  name <- trimws(as.character(brain_structure))
  if (tolower(name) %in% c("lh", "left")) {
    return("CIFTI_STRUCTURE_CORTEX_LEFT")
  }
  if (tolower(name) %in% c("rh", "right")) {
    return("CIFTI_STRUCTURE_CORTEX_RIGHT")
  }
  if (startsWith(name, "CIFTI_STRUCTURE_")) {
    return(toupper(name))
  }
  # Convert the CamelCase spelling used by nibabel and by the Workbench GUI (e.g.
  # 'CortexLeft', 'BrainStem') to the screaming snake case spelling of the CIFTI
  # standard ('CORTEX_LEFT', 'BRAIN_STEM').
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name)
  return(paste0("CIFTI_STRUCTURE_", toupper(name)))
}


#' @title Get the short name of a CIFTI brain structure.
#'
#' @description Strip the `CIFTI_STRUCTURE_` prefix from a brain structure name,
#'   normalizing it first, see \code{\link{cifti.structure.canonical}}.
#'
#' @inheritParams cifti.structure.canonical
#'
#' @return character string, the structure name without the prefix (e.g.
#'   'CORTEX_LEFT').
#'
#' @keywords internal
cifti.structure.short <- function(brain_structure) {
  return(sub("^CIFTI_STRUCTURE_", "", cifti.structure.canonical(brain_structure)))
}


#' @title Get the brain model table of a CIFTI-2 file.
#'
#' @description Get the brain model entries for one matrix dimension of a CIFTI-2
#'   file, i.e. the mapping from matrix indices to surface vertices or volume
#'   voxels. Note that a brain structure can appear in several brain model
#'   entries: a grayordinates file can contain a surface part and a volume part
#'   for the same structure, and a structure can be split into several index
#'   ranges. Always use the index ranges from this table to map data values, never
#'   the structure names.
#'
#' @param cii an `fs.cifti` instance, see \code{\link{read.cifti.header}}.
#'
#' @param dim integer, the matrix dimension to get the brain models for. CIFTI-2
#'   files have two dimensions, so this is usually 0 (Workbench calls it the ROW
#'   dimension) or 1 (the COLUMN dimension). See
#'   \code{read.cifti.header} for details on the dimensions.
#'
#' @return a data.frame with one row per brain model entry and the columns
#'   'structure' (character string, the brain structure in the spelling used in
#'   the file), 'structure_short' (character string, the normalized name without
#'   the `CIFTI_STRUCTURE_` prefix, e.g. 'CORTEX_LEFT'), 'model_type' ('SURFACE'
#'   or 'VOXELS'), 'index_offset' (integer, 0-based index of the first matrix
#'   entry covered by this brain model), 'index_count' (integer, the number of
#'   matrix entries covered) and 'surface_number_of_vertices' (integer, the
#'   number of vertices of the complete surface, for surface models; `NA` for
#'   volume models). Use `index_offset` and `index_count` to extract the data
#'   values for a brain model from the data matrix.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cifti.structures(cii, dim = 1L)
#'
#' @family cifti functions
#' @export
cifti.structures <- function(cii, dim = 0L) {
  cifti.check.object(cii)
  map <- cifti.map.for.dim(cii, dim)
  if (map$type != "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type '%s', not 'CIFTI_INDEX_TYPE_BRAIN_MODELS', so there are no brain model entries. Use cifti.parcels() for parcellated data.\n",
                 as.integer(dim), cii$filepath, map$type))
  }
  if (is.null(map$brain_models)) {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type 'CIFTI_INDEX_TYPE_BRAIN_MODELS' but contains no brain model entries.\n",
                 as.integer(dim), cii$filepath))
  }
  brain_models <- map$brain_models
  structures <- vapply(brain_models, function(bm) bm$brain_structure, character(1L))
  return(data.frame(
    structure = structures,
    structure_short = vapply(brain_models, function(bm) cifti.structure.short(bm$brain_structure), character(1L)),
    model_type = vapply(brain_models, function(bm) sub("^CIFTI_MODEL_TYPE_", "", bm$model_type), character(1L)),
    index_offset = vapply(brain_models, function(bm) as.integer(bm$index_offset), integer(1L)),
    index_count = vapply(brain_models, function(bm) as.integer(bm$index_count), integer(1L)),
    surface_number_of_vertices = vapply(brain_models, function(bm) as.integer(bm$surface_number_of_vertices), integer(1L)),
    stringsAsFactors = FALSE
  ))
}


#' @title Get the parcel table of a CIFTI-2 file.
#'
#' @description Get the parcels for one matrix dimension of a parcellated
#'   CIFTI-2 file. A parcel is a named set of surface vertices and/or volume
#'   voxels; its index is its position in the list (CIFTI-2 parcels have no index
#'   attribute). The vertex lists themselves are in the field 'vertices' of the
#'   `fs.cifti` object, see \code{\link{read.cifti.header}}.
#'
#' @inheritParams cifti.structures
#'
#' @return a data.frame with one row per parcel and the columns 'index' (integer,
#'   0-based parcel index), 'name' (character string, the parcel name), 'num_vertices'
#'   (integer, the total number of surface vertices in this parcel) and
#'   'num_voxels' (integer, the number of volume voxels in this parcel). The
#'   result is ordered by parcel index.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.ptseries.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cifti.parcels(cii, dim = 1L)
#'
#' @family cifti functions
#' @export
cifti.parcels <- function(cii, dim = 0L) {
  cifti.check.object(cii)
  map <- cifti.map.for.dim(cii, dim)
  if (map$type != "CIFTI_INDEX_TYPE_PARCELS") {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type '%s', not 'CIFTI_INDEX_TYPE_PARCELS', so there are no parcels. Use cifti.structures() for dense data.\n",
                 as.integer(dim), cii$filepath, map$type))
  }
  if (is.null(map$parcels)) {
    stop(sprintf("The mapping for dimension %d of CIFTI file '%s' is of type 'CIFTI_INDEX_TYPE_PARCELS' but contains no parcels.\n",
                 as.integer(dim), cii$filepath))
  }
  parcels <- map$parcels
  return(data.frame(
    index = vapply(parcels, function(p) as.integer(p$index), integer(1L)),
    name = vapply(parcels, function(p) as.character(p$name), character(1L)),
    num_vertices = vapply(parcels, function(p) sum(vapply(p$vertices, length, integer(1L))), integer(1L)),
    num_voxels = vapply(parcels, function(p) if (is.null(p$voxel_indices_ijk)) 0L else nrow(p$voxel_indices_ijk), integer(1L)),
    stringsAsFactors = FALSE
  ))
}


#' @title Get the series information of a CIFTI-2 file.
#'
#' @description Get the description of the series dimension of a CIFTI-2 file,
#'   e.g. the time points of a `.dtseries` or `.ptseries` file. The time (or
#'   other unit) of the series index `i` is
#'   `(start + i * step) * 10^exponent`, with `i` starting at 0.
#'
#' @inheritParams cifti.structures
#'
#' @return a named list with the entries 'number_of_series_points' (integer),
#'   'start' (numeric), 'step' (numeric), 'exponent' (integer) and 'unit'
#'   (character string, one of 'SECOND', 'HERTZ', 'METER', 'RADIAN'), or `NULL`
#'   if the specified matrix dimension is not a series. To check whether a
#'   dimension is a series, inspect the field 'type' of the corresponding entry
#'   of `cii$matrix$indices_maps`.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dtseries.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cifti.series.info(cii, dim = 0L)
#'
#' @family cifti functions
#' @export
cifti.series.info <- function(cii, dim = 0L) {
  cifti.check.object(cii)
  map <- cifti.map.for.dim(cii, dim)
  if (map$type != "CIFTI_INDEX_TYPE_SERIES") {
    return(NULL)
  }
  return(map$series)
}


#' @title Get a label table from a CIFTI-2 label file.
#'
#' @description Get the label table of one label map (a `.dlabel` file can
#'   contain several maps). The label table maps integer label keys to names and
#'   RGBA colors, and is the CIFTI analogue of a FreeSurfer color lookup table.
#'
#' @inheritParams cifti.structures
#'
#' @param map integer, the number of the label map to get the label table for
#'   (1-based). A CIFTI label file can contain several maps, use
#'   \code{cii$matrix$indices_maps} or the `map_name` field of the result to find
#'   out which one you need.
#'
#' @return a data.frame with one row per label and the columns 'key' (integer,
#'   the label key used in the data), 'red', 'green', 'blue', 'alpha' (numeric,
#'   in range 0 to 1) and 'label' (character string, the label name), plus 'x',
#'   'y', 'z' (numeric, the optional label coordinates; `NA` if not present in
#'   the file). Returns `NULL` if the specified matrix dimension does not contain
#'   label maps, or if the specified map has no label table.
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dlabel.nii", package = "freesurferformats")
#' cii <- read.cifti.header(cifti_file)
#' cifti.label.table(cii, dim = 0L, map = 1L)
#'
#' @family cifti functions
#' @export
cifti.label.table <- function(cii, dim = 0L, map = 1L) {
  cifti.check.object(cii)
  indices_map <- cifti.map.for.dim(cii, dim)
  if (is.null(indices_map$named_maps)) {
    return(NULL)
  }
  if (!is.numeric(map) || length(map) != 1L || is.na(map) || map < 1L || map > length(indices_map$named_maps)) {
    stop(sprintf("Parameter 'map' must be an integer in range 1 to %d (the number of named maps in dimension %d of file '%s'), but it is %s.\n",
                 length(indices_map$named_maps), as.integer(dim), cii$filepath, paste(as.character(map), collapse = ", ")))
  }
  return(indices_map$named_maps[[as.integer(map)]]$labels)
}


# --- Internal helpers (not exported) -----------------------------------------

#' @title Check that an object is an fs.cifti instance.
#'
#' @param cii the object to check.
#'
#' @return the object, invisibly.
#'
#' @keywords internal
cifti.check.object <- function(cii) {
  if (!(is.list(cii) && inherits(cii, "fs.cifti"))) {
    stop("Parameter 'cii' must be an fs.cifti instance, i.e. a CIFTI-2 header as returned by read.cifti.header().")
  }
  return(invisible(cii))
}


#' @title Get the CIFTI indices map for a matrix dimension.
#'
#' @description Get the single `MatrixIndicesMap` (mapping) that describes the
#'   given matrix dimension. A CIFTI-2 file can have several mappings, and one
#'   mapping can apply to several dimensions (this is the case for connectome
#'   files like `.dconn` and `.pconn`).
#'
#' @inheritParams cifti.structures
#'
#' @return the indices map, a named list, see \code{\link{read.cifti.header}}.
#'
#' @keywords internal
cifti.map.for.dim <- function(cii, dim) {
  if (!is.numeric(dim) || length(dim) != 1L || is.na(dim) || dim < 0L) {
    stop("Parameter 'dim' must be a single non-negative integer.")
  }
  dim <- as.integer(dim)
  if (dim > (length(cii$matrix$dim_sizes) - 1L)) {
    stop(sprintf("Parameter 'dim' must be in range 0 to %d for CIFTI file '%s', but it is %d.\n",
                 length(cii$matrix$dim_sizes) - 1L, cii$filepath, dim))
  }
  for (map in cii$matrix$indices_maps) {
    if (dim %in% map$dims) {
      return(map)
    }
  }
  stop(sprintf("CIFTI file '%s' contains no mapping for matrix dimension %d.\n", cii$filepath, dim)) # nocov
}


#' @title Parse the CIFTI XML metadata.
#'
#' @param xml_text character string, the XML document.
#'
#' @param filepath character string, the path of the file the XML was read from.
#'   Only used in error messages.
#'
#' @param niiheader the NIFTI-2 header of the file, required to determine the
#'   sizes of the matrix dimensions.
#'
#' @return an `fs.cifti` object, see \code{\link{read.cifti.header}}.
#'
#' @keywords internal
cifti.parse.xml <- function(xml_text, filepath = "", niiheader = NULL) {
  xml_doc <- tryCatch(
    xml2::read_xml(xml_text),
    error = function(e) {
      stop(sprintf("Failed to parse the XML metadata of CIFTI-2 file '%s': %s\n", filepath, conditionMessage(e)))
    }
  )
  xml2::xml_ns_strip(xml_doc)
  root <- xml2::xml_root(xml_doc)
  if (xml2::xml_name(root) != "CIFTI") {
    stop(sprintf("The XML metadata of CIFTI-2 file '%s' has the root element '%s', expected 'CIFTI'.\n",
                 filepath, xml2::xml_name(root)))
  }

  version <- xml2::xml_attr(root, "Version")
  if (is.na(version)) {
    stop(sprintf("The XML metadata of CIFTI-2 file '%s' has no 'Version' attribute in the 'CIFTI' element.\n", filepath))
  }
  if (!identical(version, "2")) {
    stop(sprintf(paste0(
      "File '%s' contains CIFTI version '%s', but only CIFTI-2 is supported. ",
      "Convert the file with the Connectome Workbench command ",
      "'wb_command -cifti-convert -version-convert <input> 2 <output>'.\n"), filepath, version))
  }

  matrix_node <- xml2::xml_find_first(root, "Matrix")
  if (inherits(matrix_node, "xml_missing")) {
    stop(sprintf("The XML metadata of CIFTI-2 file '%s' contains no 'Matrix' element.\n", filepath))
  }

  dim_sizes <- cifti.matrix.dim.sizes(niiheader, filepath)

  map_nodes <- xml2::xml_find_all(matrix_node, "MatrixIndicesMap")
  if (length(map_nodes) == 0L) {
    stop(sprintf("The XML metadata of CIFTI-2 file '%s' contains no 'MatrixIndicesMap' element.\n", filepath))
  }
  indices_maps <- lapply(map_nodes, function(node) cifti.parse.indices.map(node, dim_sizes, filepath))
  cifti.validate.dims.coverage(indices_maps, dim_sizes, filepath)
  for (map in indices_maps) {
    cifti.validate.indices.map(map, filepath)
  }

  cii <- list(
    filepath = filepath,
    niiheader = niiheader,
    version = version,
    matrix = list(
      metadata = cifti.parse.metadata(xml2::xml_find_first(matrix_node, "MetaData")),
      indices_maps = indices_maps,
      dim_sizes = dim_sizes
    )
  )
  class(cii) <- "fs.cifti"
  return(cii)
}


#' @title Determine the sizes of the CIFTI matrix dimensions.
#'
#' @description The CIFTI-2 matrix dimensions 0, 1, ... are stored in the
#'   `dim` field of the NIFTI-2 header, starting at its 6th entry (R index
#'   vectors are 1-based, so matrix dimension 0 is entry 6). The dimension order
#'   in the XML refers to matrix dimensions, so a `MatrixIndicesMap` with
#'   `AppliesToMatrixDimension="0"` describes the 6th entry of `dim`.
#'
#' @inheritParams cifti.parse.xml
#'
#' @return integer vector, the sizes of the matrix dimensions.
#'
#' @keywords internal
cifti.matrix.dim.sizes <- function(niiheader, filepath = "") {
  if (is.null(niiheader) || is.null(niiheader$dim) || length(niiheader$dim) != 8L) {
    stop("Parameter 'niiheader' must be a NIFTI-2 header as returned by read.nifti2.header().") # nocov
  }
  num_dims <- as.integer(niiheader$dim[1L])
  if (num_dims < 5L || num_dims > 8L) {
    stop(sprintf("CIFTI-2 file '%s' has an invalid 'dim[0]' value of %d, expected 6 (or 7 for a three-dimensional matrix).\n",
                 filepath, num_dims))
  }
  # The NIFTI dim field stores the number of dimensions in dim[0] and the sizes in
  # dim[1] to dim[dim[0]]. R vectors are 0-based with respect to the NIFTI names, so
  # 'niiheader$dim[1]' is dim[0] and the size of the CIFTI dimension 'i' is
  # 'niiheader$dim[i + 6]' (CIFTI dimension 0 is dim[5], which is the 6th entry of
  # the R vector).
  if (any(niiheader$dim[2:5] != 1L)) {
    warning(sprintf("CIFTI-2 file '%s' has non-empty dim[1] to dim[4] fields (%s), which violates the CIFTI-2 standard.\n",
                    filepath, paste(niiheader$dim[2:5], collapse = ", ")))
  }
  num_matrix_dims <- num_dims - 4L
  return(as.integer(niiheader$dim[seq.int(6L, 5L + num_matrix_dims)]))
}


#' @title Check that all matrix dimensions are described exactly once.
#'
#' @inheritParams cifti.parse.xml
#'
#' @param indices_maps list of parsed indices maps.
#'
#' @return `NULL`, invisibly. Stops if a dimension is described twice or not at
#'   all.
#'
#' @keywords internal
cifti.validate.dims.coverage <- function(indices_maps, dim_sizes, filepath = "") {
  covered <- integer(0L)
  for (map in indices_maps) {
    if (any(map$dims < 0L) || any(map$dims > (length(dim_sizes) - 1L))) {
      stop(sprintf(paste0("CIFTI-2 file '%s' contains a mapping for matrix dimension %s, ",
                          "but the file only has %d matrix dimensions (0 to %d).\n"),
                   filepath, paste(map$dims[map$dims < 0L | map$dims > (length(dim_sizes) - 1L)], collapse = ", "),
                   length(dim_sizes), length(dim_sizes) - 1L))
    }
    duplicated_dims <- intersect(covered, map$dims)
    if (length(duplicated_dims) > 0L) {
      stop(sprintf("CIFTI-2 file '%s' contains more than one mapping for matrix dimension %s, which is not allowed.\n",
                   filepath, paste(duplicated_dims, collapse = ", ")))
    }
    covered <- c(covered, map$dims)
  }
  missing_dims <- setdiff(seq_len(length(dim_sizes)) - 1L, covered)
  if (length(missing_dims) > 0L) {
    stop(sprintf("CIFTI-2 file '%s' contains no mapping for matrix dimension %s.\n",
                 filepath, paste(missing_dims, collapse = ", ")))
  }
  return(invisible(NULL))
}


#' @title Parse one MatrixIndicesMap element.
#'
#' @param node an xml2 node, the `MatrixIndicesMap` element.
#'
#' @inheritParams cifti.parse.xml
#'
#' @return a named list, see \code{\link{read.cifti.header}}.
#'
#' @keywords internal
cifti.parse.indices.map <- function(node, dim_sizes, filepath = "") {
  applies_to <- xml2::xml_attr(node, "AppliesToMatrixDimension")
  if (is.na(applies_to) || nchar(trimws(applies_to)) == 0L) {
    stop(sprintf("A 'MatrixIndicesMap' element of CIFTI-2 file '%s' has no 'AppliesToMatrixDimension' attribute.\n", filepath))
  }
  dims <- cifti.parse.int.vector(applies_to, what = "AppliesToMatrixDimension", context = sprintf("file '%s'", filepath))

  type <- xml2::xml_attr(node, "IndicesMapToDataType")
  if (is.na(type) || !(type %in% cifti.index.types())) {
    stop(sprintf("A 'MatrixIndicesMap' element of CIFTI-2 file '%s' has the invalid or unsupported 'IndicesMapToDataType' '%s'. Supported types are: %s.\n",
                 filepath, as.character(type), paste(cifti.index.types(), collapse = ", ")))
  }

  map <- list(
    applies_to = applies_to,
    dims = dims,
    type = type,
    size = as.integer(dim_sizes[dims + 1L]),
    series = cifti.parse.series(node, filepath),
    surfaces = cifti.parse.surfaces(node, filepath),
    volumes = cifti.parse.volumes(node, filepath),
    brain_models = cifti.parse.brain.models(node, filepath),
    parcels = cifti.parse.parcels(node, filepath),
    named_maps = cifti.parse.named.maps(node, filepath)
  )
  return(map)
}


#' @title Parse the MetaData element of a CIFTI XML node.
#'
#' @param node an xml2 node that may contain a `MetaData` child element.
#'
#' @return a named list of character strings, the metadata entries in file order.
#'   Empty list if the node has no metadata.
#'
#' @keywords internal
cifti.parse.metadata <- function(node) {
  metadata <- list()
  if (inherits(node, "xml_missing")) {
    return(metadata)
  }
  md_nodes <- xml2::xml_find_all(node, "MD")
  for (md_node in md_nodes) {
    name <- xml2::xml_text(xml2::xml_find_first(md_node, "Name"))
    value <- xml2::xml_text(xml2::xml_find_first(md_node, "Value"))
    metadata[[length(metadata) + 1L]] <- value
    names(metadata)[length(metadata)] <- trimws(name)
  }
  return(metadata)
}


#' @title Parse the series attributes of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a named list with the entries 'number_of_series_points', 'start',
#'   'step', 'exponent' and 'unit', or `NULL` if the element has no series
#'   attributes.
#'
#' @keywords internal
cifti.parse.series <- function(node, filepath = "") {
  num_points <- cifti.parse.attr(node, "NumberOfSeriesPoints")
  if (is.na(num_points)) {
    return(NULL)
  }
  if (is.na(cifti.parse.attr(node, "SeriesStart")) || is.na(cifti.parse.attr(node, "SeriesStep")) || is.na(cifti.parse.attr(node, "SeriesUnit"))) {
    stop(sprintf("A 'MatrixIndicesMap' element of CIFTI-2 file '%s' has 'NumberOfSeriesPoints' but is missing one of the required attributes 'SeriesStart', 'SeriesStep' or 'SeriesUnit'.\n", filepath))
  }
  return(list(
    number_of_series_points = as.integer(cifti.parse.attr(node, "NumberOfSeriesPoints")),
    start = as.numeric(cifti.parse.attr(node, "SeriesStart")),
    step = as.numeric(cifti.parse.attr(node, "SeriesStep")),
    exponent = as.integer(cifti.parse.attr(node, "SeriesExponent")),
    unit = as.character(cifti.parse.attr(node, "SeriesUnit"))
  ))
}


#' @title Parse the Surface elements of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a list of lists with the entries 'brain_structure' and
#'   'surface_number_of_vertices', or `NULL` if there are no Surface elements.
#'
#' @keywords internal
cifti.parse.surfaces <- function(node, filepath = "") {
  surface_nodes <- xml2::xml_find_all(node, "Surface")
  if (length(surface_nodes) == 0L) {
    return(NULL)
  }
  return(lapply(surface_nodes, function(surface_node) {
    return(list(
      brain_structure = xml2::xml_attr(surface_node, "BrainStructure"),
      surface_number_of_vertices = as.integer(cifti.parse.attr(surface_node, "SurfaceNumberOfVertices", required = TRUE, context = sprintf("a 'Surface' element of CIFTI-2 file '%s'", filepath)))
    ))
  }))
}


#' @title Parse the Volume elements of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a list of lists with the entries 'dimensions', 'meter_exponent' and
#'   'transformation_matrix', or `NULL` if there are no Volume elements.
#'
#' @keywords internal
cifti.parse.volumes <- function(node, filepath = "") {
  volume_nodes <- xml2::xml_find_all(node, "Volume")
  if (length(volume_nodes) == 0L) {
    return(NULL)
  }
  return(lapply(volume_nodes, function(volume_node) {
    dims <- cifti.parse.int.vector(cifti.parse.attr(volume_node, "VolumeDimensions", required = TRUE, context = sprintf("a 'Volume' element of CIFTI-2 file '%s'", filepath)),
                                   what = "VolumeDimensions", context = sprintf("a 'Volume' element of CIFTI-2 file '%s'", filepath))
    if (length(dims) != 3L) {
      stop(sprintf("A 'Volume' element of CIFTI-2 file '%s' has %d volume dimensions instead of 3.\n", filepath, length(dims)))
    }
    matrix_node <- xml2::xml_find_first(volume_node, "TransformationMatrixVoxelIndicesIJKtoXYZ")
    if (inherits(matrix_node, "xml_missing")) {
      stop(sprintf("A 'Volume' element of CIFTI-2 file '%s' has no 'TransformationMatrixVoxelIndicesIJKtoXYZ' child element, so its voxel indices cannot be interpreted.\n", filepath))
    }
    matrix_values <- cifti.parse.numeric.vector(xml2::xml_text(matrix_node), what = "TransformationMatrixVoxelIndicesIJKtoXYZ", context = sprintf("CIFTI-2 file '%s'", filepath))
    if (length(matrix_values) != 16L) {
      stop(sprintf("The 'TransformationMatrixVoxelIndicesIJKtoXYZ' element of CIFTI-2 file '%s' contains %d values instead of 16.\n", filepath, length(matrix_values)))
    }
    return(list(
      dimensions = dims,
      meter_exponent = as.integer(cifti.parse.attr(matrix_node, "MeterExponent")),
      # The 16 values are stored row by row, i.e., the first 4 values are the first row of the matrix.
      transformation_matrix = matrix(matrix_values, nrow = 4L, ncol = 4L, byrow = TRUE)
    ))
  }))
}


#' @title Parse the BrainModel elements of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a list of lists, or `NULL` if there are no BrainModel elements.
#'
#' @keywords internal
cifti.parse.brain.models <- function(node, filepath = "") {
  model_nodes <- xml2::xml_find_all(node, "BrainModel")
  if (length(model_nodes) == 0L) {
    return(NULL)
  }
  return(lapply(model_nodes, function(model_node) {
    model <- list(
      index_offset = as.integer(cifti.parse.attr(model_node, "IndexOffset", required = TRUE, context = sprintf("a 'BrainModel' element of CIFTI-2 file '%s'", filepath))),
      index_count = as.integer(cifti.parse.attr(model_node, "IndexCount", required = TRUE, context = sprintf("a 'BrainModel' element of CIFTI-2 file '%s'", filepath))),
      model_type = as.character(cifti.parse.attr(model_node, "ModelType", required = TRUE, context = sprintf("a 'BrainModel' element of CIFTI-2 file '%s'", filepath))),
      brain_structure = as.character(xml2::xml_attr(model_node, "BrainStructure")),
      surface_number_of_vertices = as.integer(cifti.parse.attr(model_node, "SurfaceNumberOfVertices"))
    )
    if (!(model$model_type %in% cifti.model.types())) {
      stop(sprintf("A 'BrainModel' element of CIFTI-2 file '%s' has the invalid or unsupported 'ModelType' '%s'. Supported types are: %s.\n",
                   filepath, model$model_type, paste(cifti.model.types(), collapse = ", ")))
    }
    model$vertex_indices <- cifti.parse.child.int.vector(model_node, "VertexIndices", filepath)
    model$voxel_indices_ijk <- cifti.parse.child.voxel.indices(model_node, filepath)
    return(model)
  }))
}


#' @title Parse the Parcel elements of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a list of lists, or `NULL` if there are no Parcel elements.
#'
#' @keywords internal
cifti.parse.parcels <- function(node, filepath = "") {
  parcel_nodes <- xml2::xml_find_all(node, "Parcel")
  if (length(parcel_nodes) == 0L) {
    return(NULL)
  }
  return(lapply(seq_along(parcel_nodes), function(parcel_idx) {
    parcel_node <- parcel_nodes[[parcel_idx]]
    vertices <- list()
    for (vertex_node in xml2::xml_find_all(parcel_node, "Vertices")) {
      brain_structure <- xml2::xml_attr(vertex_node, "BrainStructure")
      if (is.na(brain_structure)) {
        stop(sprintf("A 'Vertices' element of parcel %d of CIFTI-2 file '%s' has no 'BrainStructure' attribute.\n", parcel_idx - 1L, filepath))
      }
      vertices[[cifti.structure.short(brain_structure)]] <- cifti.parse.int.vector(
        xml2::xml_text(vertex_node), what = "Vertices", context = sprintf("parcel %d of CIFTI-2 file '%s'", parcel_idx - 1L, filepath))
    }
    return(list(
      index = as.integer(parcel_idx - 1L), # CIFTI indices are 0-based, and a parcel has no index attribute: it is the position in the list.
      name = as.character(xml2::xml_attr(parcel_node, "Name")),
      vertices = vertices,
      voxel_indices_ijk = cifti.parse.child.voxel.indices(parcel_node, filepath)
    ))
  }))
}


#' @title Parse the NamedMap elements of a MatrixIndicesMap element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return a list of lists with the entries 'name', 'metadata' and 'labels', or
#'   `NULL` if there are no NamedMap elements.
#'
#' @keywords internal
cifti.parse.named.maps <- function(node, filepath = "") {
  named_map_nodes <- xml2::xml_find_all(node, "NamedMap")
  if (length(named_map_nodes) == 0L) {
    return(NULL)
  }
  return(lapply(named_map_nodes, function(named_map_node) {
    label_table_node <- xml2::xml_find_first(named_map_node, "LabelTable")
    return(list(
      name = trimws(xml2::xml_text(xml2::xml_find_first(named_map_node, "MapName"))),
      metadata = cifti.parse.metadata(xml2::xml_find_first(named_map_node, "MetaData")),
      labels = cifti.parse.label.table(label_table_node)
    ))
  }))
}


#' @title Parse a CIFTI LabelTable element.
#'
#' @param node an xml2 node, the `LabelTable` element, or an `xml_missing` node.
#'
#' @return a data.frame with the columns 'key', 'red', 'green', 'blue', 'alpha',
#'   'label', 'x', 'y' and 'z', or `NULL` if the node is missing.
#'
#' @keywords internal
cifti.parse.label.table <- function(node) {
  if (inherits(node, "xml_missing")) {
    return(NULL)
  }
  label_nodes <- xml2::xml_find_all(node, "Label")
  if (length(label_nodes) == 0L) {
    return(NULL)
  }
  label_attr <- function(label_node, attr_name, numeric = TRUE) {
    value <- xml2::xml_attr(label_node, attr_name)
    if (is.na(value)) {
      return(if (numeric) NA_real_ else NA_character_)
    }
    return(if (numeric) as.numeric(value) else value)
  }
  return(data.frame(
    key = vapply(label_nodes, function(n) as.integer(cifti.parse.attr(n, "Key", required = TRUE, context = "a 'Label' element")), integer(1L)),
    red = vapply(label_nodes, function(n) label_attr(n, "Red"), numeric(1L)),
    green = vapply(label_nodes, function(n) label_attr(n, "Green"), numeric(1L)),
    blue = vapply(label_nodes, function(n) label_attr(n, "Blue"), numeric(1L)),
    alpha = vapply(label_nodes, function(n) label_attr(n, "Alpha"), numeric(1L)),
    label = vapply(label_nodes, function(n) trimws(xml2::xml_text(n)), character(1L)),
    x = vapply(label_nodes, function(n) label_attr(n, "X"), numeric(1L)),
    y = vapply(label_nodes, function(n) label_attr(n, "Y"), numeric(1L)),
    z = vapply(label_nodes, function(n) label_attr(n, "Z"), numeric(1L)),
    stringsAsFactors = FALSE
  ))
}


#' @title Parse a required or optional child element holding integer values.
#'
#' @param node an xml2 node, the parent element.
#'
#' @param child_name character string, the name of the child element.
#'
#' @inheritParams cifti.parse.indices.map
#'
#' @return integer vector, or `NULL` if the child element is missing or empty.
#'
#' @keywords internal
cifti.parse.child.int.vector <- function(node, child_name, filepath = "") {
  child_node <- xml2::xml_find_first(node, child_name)
  if (inherits(child_node, "xml_missing")) {
    return(NULL)
  }
  values <- cifti.parse.int.vector(xml2::xml_text(child_node), what = child_name, context = sprintf("CIFTI-2 file '%s'", filepath))
  if (length(values) == 0L) {
    return(NULL)
  }
  return(values)
}


#' @title Parse the optionally present VoxelIndicesIJK child element as a matrix.
#'
#' @inheritParams cifti.parse.child.int.vector
#'
#' @return an n x 3 integer matrix of 0-based voxel indices, or `NULL` if the
#'   child element is missing or empty.
#'
#' @keywords internal
cifti.parse.child.voxel.indices <- function(node, filepath = "") {
  values <- cifti.parse.child.int.vector(node, "VoxelIndicesIJK", filepath = filepath)
  if (is.null(values)) {
    return(NULL)
  }
  if ((length(values) %% 3L) != 0L) {
    stop(sprintf("A 'VoxelIndicesIJK' element of CIFTI-2 file '%s' contains %d values, which is not a multiple of 3.\n",
                 filepath, length(values)))
  }
  return(matrix(values, ncol = 3L, byrow = TRUE))
}


#' @title Parse a whitespace-separated list of integers from XML text content.
#'
#' @param text character string, the text content of an XML element.
#'
#' @param what character string, the element/attribute name, used in error messages.
#'
#' @param context character string, additional context for error messages.
#'
#' @return integer vector. Empty integer vector for empty input.
#'
#' @keywords internal
cifti.parse.int.vector <- function(text, what = "an element", context = "") {
  if (is.na(text)) {
    stop(sprintf("Failed to parse %s (%s): the value is missing.\n", what, context)) # nocov
  }
  # The numbers are whitespace separated. Different software uses different line
  # breaks: nibabel writes all values of an element on one line, Connectome
  # Workbench uses one line per voxel. Splitting on any whitespace handles both.
  # Commas are also accepted as separators, because attributes that hold
  # dimension lists use them, e.g. AppliesToMatrixDimension="0,1" and
  # VolumeDimensions="4,4,4".
  tokens <- strsplit(trimws(text), "[[:space:],]+")[[1L]]
  if (length(tokens) == 1L && nchar(tokens[1L]) == 0L) {
    return(integer(0L))
  }
  values <- suppressWarnings(as.integer(tokens))
  if (any(is.na(values))) {
    stop(sprintf("Failed to parse %s (%s): the value '%s' is not an integer.\n",
                 what, context, tokens[which(is.na(values))[1L]]))
  }
  return(values)
}


#' @title Parse a whitespace-separated list of numbers from XML text content.
#'
#' @inheritParams cifti.parse.int.vector
#'
#' @return numeric vector. Empty numeric vector for empty input.
#'
#' @keywords internal
cifti.parse.numeric.vector <- function(text, what = "an element", context = "") {
  tokens <- strsplit(trimws(text), "[[:space:],]+")[[1L]]
  if (length(tokens) == 1L && nchar(tokens[1L]) == 0L) {
    return(numeric(0L))
  }
  values <- suppressWarnings(as.numeric(tokens))
  if (any(is.na(values))) {
    stop(sprintf("Failed to parse %s (%s): the value '%s' is not a number.\n",
                 what, context, tokens[which(is.na(values))[1L]]))
  }
  return(values)
}


#' @title Read and validate an attribute value of an XML node.
#'
#' @param node an xml2 node.
#'
#' @param attr_name character string, the name of the attribute.
#'
#' @param required logical, whether the attribute must be present.
#'
#' @param context character string, used in the error message.
#'
#' @return character string, or `NA_character_` if the attribute is absent and
#'   not required.
#'
#' @keywords internal
cifti.parse.attr <- function(node, attr_name, required = FALSE, context = "") {
  value <- xml2::xml_attr(node, attr_name)
  if (is.na(value) && required) {
    stop(sprintf("Failed to parse %s: the required attribute '%s' is missing.\n", context, attr_name))
  }
  return(value)
}


#' @title Validate a parsed CIFTI MatrixIndicesMap.
#'
#' @description Check that the mapping is consistent with the size of the matrix
#'   dimension it describes, and that the index ranges it declares are valid.
#'   These checks exist because a mismatch would silently misalign the data.
#'
#' @param map a parsed indices map, see \code{\link{cifti.parse.indices.map}}.
#'
#' @param filepath character string, the file path, used in error messages.
#'
#' @return `NULL`, invisibly. Stops with a descriptive error if the mapping is
#'   inconsistent.
#'
#' @keywords internal
cifti.validate.indices.map <- function(map, filepath = "") {
  if (length(unique(map$size)) > 1L) {
    stop(sprintf("CIFTI-2 file '%s' contains a mapping that applies to matrix dimensions %s of different sizes (%s). This is not allowed.\n",
                 filepath, paste(map$dims, collapse = ", "), paste(map$size, collapse = " and ")))
  }
  dim_size <- map$size[1L]

  if (map$type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    if (is.null(map$brain_models)) {
      stop(sprintf("CIFTI-2 file '%s' contains a 'CIFTI_INDEX_TYPE_BRAIN_MODELS' mapping for matrix dimension %s without any 'BrainModel' entries.\n",
                   filepath, paste(map$dims, collapse = ", ")))
    }
    cifti.validate.brain.models(map, dim_size, filepath)
  } else if (map$type == "CIFTI_INDEX_TYPE_PARCELS") {
    if (is.null(map$parcels)) {
      stop(sprintf("CIFTI-2 file '%s' contains a 'CIFTI_INDEX_TYPE_PARCELS' mapping for matrix dimension %s without any 'Parcel' entries.\n",
                   filepath, paste(map$dims, collapse = ", ")))
    }
    if (length(map$parcels) != dim_size) {
      stop(sprintf("CIFTI-2 file '%s' declares %d parcels for matrix dimension %s, but the dimension has size %d.\n",
                   filepath, length(map$parcels), paste(map$dims, collapse = ", "), dim_size))
    }
    cifti.validate.parcels(map, filepath)
  } else if (map$type == "CIFTI_INDEX_TYPE_SERIES") {
    if (is.null(map$series)) {
      stop(sprintf("CIFTI-2 file '%s' contains a 'CIFTI_INDEX_TYPE_SERIES' mapping for matrix dimension %s without series attributes.\n",
                   filepath, paste(map$dims, collapse = ", ")))
    }
    if (map$series$number_of_series_points != dim_size) {
      stop(sprintf("CIFTI-2 file '%s' declares %d series points for matrix dimension %s, but the dimension has size %d.\n",
                   filepath, map$series$number_of_series_points, paste(map$dims, collapse = ", "), dim_size))
    }
  } else if (map$type == "CIFTI_INDEX_TYPE_LABELS") {
    if (is.null(map$named_maps)) {
      stop(sprintf("CIFTI-2 file '%s' contains a 'CIFTI_INDEX_TYPE_LABELS' mapping for matrix dimension %s without any 'NamedMap' entries, but every label map needs a label table.\n",
                   filepath, paste(map$dims, collapse = ", ")))
    }
    if (length(map$named_maps) != dim_size) {
      stop(sprintf("CIFTI-2 file '%s' declares %d label maps for matrix dimension %s, but the dimension has size %d.\n",
                   filepath, length(map$named_maps), paste(map$dims, collapse = ", "), dim_size))
    }
  } else if (map$type == "CIFTI_INDEX_TYPE_SCALARS") {
    # NamedMap elements are optional for scalars, but if present there should be one per scalar.
    if (!is.null(map$named_maps) && length(map$named_maps) != dim_size) {
      if (length(map$named_maps) > dim_size) {
        stop(sprintf("CIFTI-2 file '%s' declares %d named maps for matrix dimension %s, but the dimension has size %d.\n",
                     filepath, length(map$named_maps), paste(map$dims, collapse = ", "), dim_size))
      }
      warning(sprintf("CIFTI-2 file '%s' declares %d named maps for matrix dimension %s of size %d, so some maps are unnamed.\n",
                      filepath, length(map$named_maps), paste(map$dims, collapse = ", "), dim_size))
    }
  }
  return(invisible(NULL))
}


#' @title Validate the brain model entries of a CIFTI mapping.
#'
#' @inheritParams cifti.validate.indices.map
#'
#' @return `NULL`, invisibly. Stops with a descriptive error if an entry is
#'   inconsistent.
#'
#' @keywords internal
cifti.validate.brain.models <- function(map, dim_size, filepath = "") {
  expected_offset <- 0L
  for (model in map$brain_models) {
    if (is.na(model$index_offset) || is.na(model$index_count) || model$index_count <= 0L) {
      stop(sprintf("CIFTI-2 file '%s' contains a brain model for structure '%s' with an invalid index offset or count (%s, %s).\n",
                   filepath, model$brain_structure, as.character(model$index_offset), as.character(model$index_count)))
    }
    if (model$index_offset != expected_offset) {
      stop(sprintf(paste0("CIFTI-2 file '%s' is inconsistent: the brain model for structure '%s' starts at index %d ",
                          "but the previous brain models cover the indices up to %d. The index ranges of the brain models must ",
                          "cover the matrix dimension without gaps or overlaps.\n"),
                   filepath, model$brain_structure, model$index_offset, expected_offset - 1L))
    }
    expected_offset <- model$index_offset + model$index_count
  }
  if (expected_offset != dim_size) {
    stop(sprintf("CIFTI-2 file '%s' is inconsistent: the brain models cover %d matrix entries, but the matrix dimension has size %d.\n",
                 filepath, expected_offset, dim_size))
  }

  for (model in map$brain_models) {
    if (is.na(model$brain_structure) || !startsWith(model$brain_structure, "CIFTI_STRUCTURE_")) {
      stop(sprintf("CIFTI-2 file '%s' contains a brain model with the invalid brain structure '%s'.\n",
                   filepath, as.character(model$brain_structure)))
    }
    if (model$model_type == "CIFTI_MODEL_TYPE_SURFACE") {
      if (is.na(model$surface_number_of_vertices)) {
        stop(sprintf("CIFTI-2 file '%s' contains a surface brain model for structure '%s' without the required 'SurfaceNumberOfVertices' attribute.\n",
                     filepath, model$brain_structure))
      }
      if (is.null(model$vertex_indices)) {
        if (model$index_count != model$surface_number_of_vertices) {
          stop(sprintf(paste0("CIFTI-2 file '%s' contains a surface brain model for structure '%s' with %d indices but no 'VertexIndices' element. ",
                              "Without vertex indices, the model must cover all %d vertices of the surface.\n"),
                       filepath, model$brain_structure, model$index_count, model$surface_number_of_vertices))
        }
      } else {
        if (length(model$vertex_indices) != model$index_count) {
          stop(sprintf("CIFTI-2 file '%s' contains %d vertex indices for the brain model of structure '%s', but the model declares %d indices.\n",
                       filepath, length(model$vertex_indices), model$brain_structure, model$index_count))
        }
        cifti.validate.index.range(model$vertex_indices, model$surface_number_of_vertices - 1L,
                                   what = sprintf("vertex indices of the brain model for structure '%s'", model$brain_structure), filepath = filepath)
      }
    } else if (model$model_type == "CIFTI_MODEL_TYPE_VOXELS") {
      if (is.null(map$volumes)) {
        stop(sprintf(paste0("CIFTI-2 file '%s' contains a volume brain model for structure '%s', but the mapping has no 'Volume' element, ",
                            "so the voxel indices cannot be interpreted.\n"), filepath, model$brain_structure))
      }
      volume_dims <- map$volumes[[1L]]$dimensions
      if (is.null(model$voxel_indices_ijk)) {
        if (model$index_count != prod(volume_dims)) {
          stop(sprintf(paste0("CIFTI-2 file '%s' contains a volume brain model for structure '%s' with %d indices but no 'VoxelIndicesIJK' element. ",
                              "Without voxel indices, the model must cover all %d voxels of the volume.\n"),
                       filepath, model$brain_structure, model$index_count, prod(volume_dims)))
        }
      } else {
        if (nrow(model$voxel_indices_ijk) != model$index_count) {
          stop(sprintf("CIFTI-2 file '%s' contains %d voxel indices for the brain model of structure '%s', but the model declares %d indices.\n",
                       filepath, nrow(model$voxel_indices_ijk), model$brain_structure, model$index_count))
        }
        for (coord_idx in 1:3) {
          cifti.validate.index.range(model$voxel_indices_ijk[, coord_idx], volume_dims[coord_idx] - 1L,
                                     what = sprintf("voxel indices of the brain model for structure '%s'", model$brain_structure), filepath = filepath)
        }
      }
    }
  }
  return(invisible(NULL))
}


#' @title Validate the parcels of a CIFTI mapping.
#'
#' @inheritParams cifti.validate.indices.map
#'
#' @return `NULL`, invisibly. Stops with a descriptive error if a parcel refers
#'   to vertices outside of a declared surface.
#'
#' @keywords internal
cifti.validate.parcels <- function(map, filepath = "") {
  surface_vert_counts <- cifti.surface.vertex.counts(map)
  for (parcel in map$parcels) {
    for (structure_short in names(parcel$vertices)) {
      if (!(structure_short %in% names(surface_vert_counts))) {
        warning(sprintf(paste0("CIFTI-2 file '%s': parcel '%s' contains vertices for structure '%s', but the mapping declares no ",
                               "surface with that structure, so the vertex indices cannot be validated.\n"),
                        filepath, parcel$name, structure_short))
        next
      }
      cifti.validate.index.range(parcel$vertices[[structure_short]], surface_vert_counts[[structure_short]] - 1L,
                                 what = sprintf("vertex indices of parcel '%s'", parcel$name), filepath = filepath)
    }
  }
  return(invisible(NULL))
}


#' @title Get the number of vertices of the surfaces declared in a CIFTI mapping.
#'
#' @param map a parsed indices map, see \code{\link{cifti.parse.indices.map}}.
#'
#' @return named integer vector, the number of vertices per canonical brain
#'   structure short name (e.g. 'CORTEX_LEFT'). Empty if the mapping declares no
#'   surfaces.
#'
#' @keywords internal
cifti.surface.vertex.counts <- function(map) {
  if (is.null(map$surfaces)) {
    return(integer(0L))
  }
  counts <- vapply(map$surfaces, function(surface) as.integer(surface$surface_number_of_vertices), integer(1L))
  names(counts) <- vapply(map$surfaces, function(surface) cifti.structure.short(surface$brain_structure), character(1L))
  return(counts)
}


#' @title Check that all indices are within a valid range.
#'
#' @param indices integer vector, the indices to check.
#'
#' @param max_index integer, the largest allowed index.
#'
#' @param what character string, a description of what the indices are, used in
#'   the error message.
#'
#' @inheritParams cifti.validate.indices.map
#'
#' @return `NULL`, invisibly. Stops with a descriptive error if an index is out
#'   of range.
#'
#' @keywords internal
cifti.validate.index.range <- function(indices, max_index, what, filepath = "") {
  if (any(is.na(indices)) || any(indices < 0L) || any(indices > max_index)) {
    stop(sprintf("CIFTI-2 file '%s' is inconsistent: the %s are out of range (valid are 0 to %d, but found %s).\n",
                 filepath, what, max_index,
                 paste(utils::head(indices[is.na(indices) | indices < 0L | indices > max_index], 10L), collapse = ", ")))
  }
  return(invisible(NULL))
}


#' @title Check whether a file looks like a CIFTI-1 file.
#'
#' @description This is a heuristic used to produce a helpful error message when
#'   a CIFTI-1 file is passed to the reader: CIFTI-1 files are NIFTI-1 files (not
#'   NIFTI-2), and this function searches the first bytes of the file for the
#'   string 'CIFTI'. It is not used for any other purpose.
#'
#' @inheritParams read.cifti.header
#'
#' @return logical, whether the file looks like a CIFTI-1 file.
#'
#' @keywords internal
cifti.file.looks.like.cifti1 <- function(filepath) {
  fh <- tryCatch(file(filepath, "rb"), error = function(e) NULL)
  if (is.null(fh)) {
    return(FALSE) # nocov
  }
  on.exit(close(fh), add = TRUE)
  header_bytes <- tryCatch(readBin(fh, raw(), n = 65536L), error = function(e) raw(0L))
  if (length(header_bytes) == 0L) {
    return(FALSE) # nocov
  }
  return(length(grepRaw("CIFTI", header_bytes, fixed = TRUE)) > 0L)
}
