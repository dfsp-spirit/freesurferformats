# Functions for writing CIFTI-2 files (the CIFTI-2 XML writer and the axis helpers).
#
# The reader is in R/read_cifti.R and R/read_cifti_header.R. The axis objects used here
# are the same structures that the reader returns for the entries of
# 'cii$matrix$indices_maps', so that a header can be read, modified and written back, and
# so that a template file can be used to define the mapping of a new file (which is the
# recommended way to write an HCP grayordinates file: the mapping cannot be invented).
#
# The rules of the format that the writer has to follow (all verified against Connectome
# Workbench 2.2.1 and nibabel 5.4.2, see the plan):
#  - one MatrixIndicesMap element per dimension, except that the same element is written
#    once with AppliesToMatrixDimension="0,1" when both dimensions have identical axes
#    (this is what Workbench does for dconn/pconn files),
#  - IndexOffset/IndexCount are computed from the index lists, they must cover the
#    dimension without gaps,
#  - VoxelIndicesIJK is written one voxel per line (Workbench style), which every
#    implementation accepts, while nibabel writes all values on a single line,
#  - a volume brain model needs a Volume element with the grid dimensions and the
#    transformation matrix, which is why the voxel indices can not be interpreted without
#    it,
#  - the data are stored in the order of the matrix dimensions, dimension 0 varying
#    fastest, which is exactly R's column major order for the array the reader returns.


# The table below was verified against the files Connectome Workbench writes (see the
# fixtures in inst/extdata/cifti, which the generator script dumps): `.dpconn.nii` is
# (BRAIN_MODELS, PARCELS) with intent 3009 `ConnParcelDense`, and `.pdconn.nii` is
# (PARCELS, BRAIN_MODELS) with intent 3010 `ConnDenseParcel`. That is the opposite of
# what the two file type names suggest, which is the trap this format is known for: the
# dimension order is always taken from the axes, never from the file name.

#' @title The standard CIFTI-2 file types.
#'
#' @description The nine standard CIFTI-2 file types, as defined by the format: which
#'   matrix index types the two matrix dimensions must have, the file name extension,
#'   and the NIFTI intent code and name that identify the type in the file header. The
#'   extension of a file decides which entry is used when writing, and a mismatch
#'   between the file name and the axes of the data is an error.
#'
#' @return a data.frame with one row per file type and the columns 'extension',
#'   'intent_code', 'intent_name', 'dim0_type' and 'dim1_type'.
#'
#' @keywords internal
cifti.file.types <- function() {
  return(data.frame(
    extension = c(".dconn.nii", ".dtseries.nii", ".pconn.nii", ".ptseries.nii", ".dscalar.nii",
                  ".dlabel.nii", ".pscalar.nii", ".dpconn.nii", ".pdconn.nii"),
    intent_code = c(3001L, 3002L, 3003L, 3004L, 3006L, 3007L, 3008L, 3009L, 3010L),
    intent_name = c("ConnDense", "ConnDenseSeries", "ConnParcels", "ConnParcelSries", "ConnDenseScalar",
                    "ConnDenseLabel", "ConnParcelScalr", "ConnParcelDense", "ConnDenseParcel"),
    dim0_type = c("CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_SERIES", "CIFTI_INDEX_TYPE_PARCELS",
                  "CIFTI_INDEX_TYPE_SERIES", "CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS",
                  "CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_PARCELS"),
    dim1_type = c("CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_PARCELS",
                  "CIFTI_INDEX_TYPE_PARCELS", "CIFTI_INDEX_TYPE_BRAIN_MODELS", "CIFTI_INDEX_TYPE_BRAIN_MODELS",
                  "CIFTI_INDEX_TYPE_PARCELS", "CIFTI_INDEX_TYPE_PARCELS", "CIFTI_INDEX_TYPE_BRAIN_MODELS"),
    stringsAsFactors = FALSE
  ))
}


# --- Axis builders -----------------------------------------------------------

#' @title Create a CIFTI-2 axis for a volume.
#'
#' @description A volume axis describes the voxel grid that the volume brain models of a
#'   CIFTI-2 file refer to: its dimensions and the 4x4 transformation matrix that maps the
#'   (0-based) IJK voxel indices to coordinates. The matrix is stored row by row in the
#'   file, and the coordinates it produces are in units of `10^meter_exponent` (which is
#'   -3, i.e. millimeters, for the files that Connectome Workbench writes).
#'
#' @param dimensions integer vector of length 3, the dimensions of the voxel grid.
#'
#' @param transformation_matrix 4x4 numeric matrix, the transformation from (0-based)
#'   voxel indices to coordinates. There is no default: a CIFTI-2 file does not store the
#'   voxel size or the position of a volume anywhere else, so a wrong or invented matrix
#'   silently puts the voxels of a structure in the wrong place. Pass the matrix that the
#'   template file has (see \code{\link{cifti.axis.from.template}}) if you do not know it.
#'
#' @param meter_exponent integer, the exponent of the unit of the coordinates.
#'
#' @return a named list with the entries 'dimensions', 'meter_exponent' and
#'   'transformation_matrix', to be passed to \code{\link{cifti.axis.brain.models}} or
#'   \code{\link{cifti.axis.parcels}}.
#'
#' @examples
#' volume <- cifti.volume(c(4L, 4L, 4L), diag(c(2, 2, 2, 1)))
#' volume$dimensions
#'
#' @family cifti functions
#' @export
cifti.volume <- function(dimensions, transformation_matrix, meter_exponent = -3L) {
  if (!is.numeric(dimensions) || length(dimensions) != 3L || any(is.na(dimensions))) {
    stop("Parameter 'dimensions' must be a numeric vector of length 3.")
  }
  if (!is.matrix(transformation_matrix) || !is.numeric(transformation_matrix) ||
      !identical(dim(transformation_matrix), c(4L, 4L))) {
    stop("Parameter 'transformation_matrix' must be a numeric 4x4 matrix.")
  }
  if (!is.numeric(meter_exponent) || length(meter_exponent) != 1L || is.na(meter_exponent)) {
    stop("Parameter 'meter_exponent' must be a single integer.")
  }
  return(list(
    dimensions = as.integer(dimensions),
    meter_exponent = as.integer(meter_exponent),
    transformation_matrix = matrix(as.numeric(transformation_matrix), nrow = 4L, ncol = 4L)
  ))
}


#' @title Create a CIFTI-2 brain model entry for a surface.
#'
#' @description A surface brain model entry describes a set of surface vertices that one
#'   matrix dimension of a CIFTI-2 file contains, e.g. one hemisphere. The vertex indices
#'   are 0-based and refer to the vertices of the surface mesh that the file is defined
#'   on. If they are `NULL`, the model covers all vertices of the surface, which is only
#'   allowed if their number matches the size of the index range (the reader validates
#'   this, and the writer computes the range from the index list).
#'
#' @param structure character string, the brain structure, see
#'   \code{\link{cifti.structure.canonical}} for the accepted spellings.
#'
#' @param surface_number_of_vertices integer, the number of vertices of the complete
#'   surface (not the number of vertices in this model: a grayordinates file of the HCP
#'   leaves out the medial wall, so its models usually cover fewer vertices than the
#'   surface has).
#'
#' @param vertices integer vector or `NULL`, the 0-based vertex indices in this model.
#'
#' @return a named list, one brain model entry, to be passed to
#'   \code{\link{cifti.axis.brain.models}}.
#'
#' @examples
#' model <- cifti.brain.model.surface("lh", 10L, vertices = 0:4)
#' model$index_count
#'
#' @family cifti functions
#' @export
cifti.brain.model.surface <- function(structure, surface_number_of_vertices, vertices = NULL) {
  if (!is.numeric(surface_number_of_vertices) || length(surface_number_of_vertices) != 1L ||
      is.na(surface_number_of_vertices) || surface_number_of_vertices <= 0) {
    stop("Parameter 'surface_number_of_vertices' must be a single positive integer.")
  }
  if (!is.null(vertices)) {
    vertices <- cifti.check.index.list(vertices, "vertices", max_index = surface_number_of_vertices - 1L)
  }
  return(list(
    brain_structure = cifti.structure.canonical(structure),
    model_type = "CIFTI_MODEL_TYPE_SURFACE",
    surface_number_of_vertices = as.integer(surface_number_of_vertices),
    vertex_indices = vertices,
    voxel_indices_ijk = NULL
  ))
}


#' @title Create a CIFTI-2 brain model entry for volume voxels.
#'
#' @description A volume brain model entry describes the voxels of one subcortical
#'   structure, as 0-based IJK indices into the voxel grid of the volume that is part of
#'   the same axis (see \code{\link{cifti.volume}}). If the indices are `NULL`, the model
#'   covers all voxels of that grid.
#'
#' @param structure character string, the brain structure, see
#'   \code{\link{cifti.structure.canonical}} for the accepted spellings.
#'
#' @param voxel_indices_ijk n x 3 integer matrix or `NULL`, the 0-based voxel indices, one
#'   voxel per row. A vector of length 3*n is accepted as well and interpreted as the
#'   rows.
#'
#' @return a named list, one brain model entry, to be passed to
#'   \code{\link{cifti.axis.brain.models}}.
#'
#' @examples
#' voxels <- matrix(c(0L, 0L, 2L, 1L, 0L, 2L), ncol = 3L, byrow = TRUE)
#' model <- cifti.brain.model.volume("CEREBELLUM", voxels)
#' model$index_count
#'
#' @family cifti functions
#' @export
cifti.brain.model.volume <- function(structure, voxel_indices_ijk = NULL) {
  voxels <- NULL
  if (!is.null(voxel_indices_ijk)) {
    if (is.matrix(voxel_indices_ijk)) {
      if (ncol(voxel_indices_ijk) != 3L) {
        stop("Parameter 'voxel_indices_ijk' must have one row per voxel and 3 columns (i, j, k).")
      }
      voxels <- matrix(as.integer(voxel_indices_ijk), nrow = nrow(voxel_indices_ijk), ncol = 3L)
    } else {
      voxel_indices_ijk <- as.integer(voxel_indices_ijk)
      if (length(voxel_indices_ijk) %% 3L != 0L) {
        stop("Parameter 'voxel_indices_ijk' must contain 3 values (i, j, k) per voxel.")
      }
      voxels <- matrix(voxel_indices_ijk, ncol = 3L, byrow = TRUE)
    }
    if (any(is.na(voxels)) || any(voxels < 0L)) {
      stop("Parameter 'voxel_indices_ijk' must contain non-negative, non-NA 0-based voxel indices.")
    }
  }
  return(list(
    brain_structure = cifti.structure.canonical(structure),
    model_type = "CIFTI_MODEL_TYPE_VOXELS",
    surface_number_of_vertices = NA_integer_,
    vertex_indices = NULL,
    voxel_indices_ijk = voxels
  ))
}


#' @title Create a CIFTI-2 axis for brain models.
#'
#' @description A brain model axis describes which surface vertices and volume voxels a
#'   matrix dimension of a CIFTI-2 file contains, in the order in which they appear in the
#'   matrix. This is the mapping of a dense file (`.dscalar`, `.dtseries`, `.dlabel`,
#'   `.dconn`), and of the files that mix dense and parcellated data.
#'
#'   The index ranges of the models are computed from the index lists, so the models cover
#'   the dimension without gaps, which the format requires. A structure may appear in
#'   several models (e.g. as a surface and as a volume part in a grayordinates file), and
#'   the order of the models is the order of the matrix indices.
#'
#' @param models list of brain model entries, as created by
#'   \code{\link{cifti.brain.model.surface}} and \code{\link{cifti.brain.model.volume}}.
#'
#' @param surfaces named integer vector or `NULL`, the number of vertices of the complete
#'   surfaces the file refers to, named by brain structure (e.g.
#'   `c(CORTEX_LEFT = 32492, CORTEX_RIGHT = 32492)`). This is written as the optional
#'   `Surface` elements of the axis. Connectome Workbench does not write them, and the
#'   surface size is then taken from the brain model entries only, so this can be left at
#'   `NULL` unless the file has to state the surface sizes explicitly.
#'
#' @param volume a volume, see \code{\link{cifti.volume}}, or `NULL`. Required if any of
#'   the models is a volume model, because the voxel indices of a model can not be
#'   interpreted without the voxel grid and the transformation matrix.
#'
#' @return a named list with the entries 'type', 'brain_models', 'surfaces' and 'volumes',
#'   an axis to be passed to \code{\link{cifti.header.from.axes}} or
#'   \code{\link{write.cifti}}.
#'
#' @examples
#' axis <- cifti.axis.brain.models(list(
#'   cifti.brain.model.surface("lh", 10L),
#'   cifti.brain.model.surface("rh", 12L)))
#' axis$type
#'
#' @family cifti functions
#' @export
cifti.axis.brain.models <- function(models, surfaces = NULL, volume = NULL) {
  if (!is.list(models) || length(models) == 0L) {
    stop("Parameter 'models' must be a list of brain model entries, see cifti.brain.model.surface().")
  }
  if (!is.list(models[[1L]])) {
    models <- list(models) # a single model entry was passed
  }
  if (!is.null(volume) && !is.null(volume$dimensions)) {
    volume <- cifti.volume(volume$dimensions, volume$transformation_matrix, volume$meter_exponent)
  } else if (!is.null(volume)) {
    stop("Parameter 'volume' must be NULL or a volume as created by cifti.volume().")
  }
  models <- cifti.compute.index.ranges(models, volume = volume)
  has_volume_models <- any(vapply(models, function(model) {
    return(identical(model$model_type, "CIFTI_MODEL_TYPE_VOXELS"))
  }, logical(1L)))
  if (has_volume_models && is.null(volume)) {
    stop("A brain model axis that contains volume models needs the volume they refer to, see cifti.volume(). The voxel indices of a volume model can not be interpreted without it.")
  }
  return(list(
    type = "CIFTI_INDEX_TYPE_BRAIN_MODELS",
    brain_models = models,
    surfaces = cifti.axis.surfaces(surfaces),
    volumes = if (is.null(volume)) NULL else list(volume)
  ))
}


#' @title Create a CIFTI-2 parcel.
#'
#' @description Create one parcel, i.e. a named set of surface vertices (per structure) and
#'   volume voxels. The name is the only label a parcellated file has: the position in the
#'   list of parcels is the parcel index, and the reader returns the names.
#'
#' @param name character string, the parcel name.
#'
#' @param vertices named list or `NULL`, the 0-based vertex indices per brain structure,
#'   e.g. `list(CORTEX_LEFT = c(0, 1, 2), CORTEX_RIGHT = c(4, 5))`. Structures without
#'   vertices are left out.
#'
#' @param voxel_indices_ijk n x 3 integer matrix or `NULL`, the 0-based voxel indices of
#'   the parcel, one voxel per row.
#'
#' @return a named list, one parcel, to be passed to \code{\link{cifti.axis.parcels}}.
#'
#' @examples
#' parcel <- cifti.parcel("PARCEL_A", list(CORTEX_LEFT = 0:2, CORTEX_RIGHT = 4:5))
#' parcel$name
#'
#' @family cifti functions
#' @export
cifti.parcel <- function(name, vertices = NULL, voxel_indices_ijk = NULL) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("Parameter 'name' must be a single non-empty character string.")
  }
  parcel_vertices <- list()
  if (!is.null(vertices)) {
    if (!is.list(vertices)) {
      stop("Parameter 'vertices' must be a named list of 0-based vertex index vectors, one entry per brain structure.")
    }
    if (is.null(names(vertices))) {
      stop("Parameter 'vertices' must be a named list, with the brain structure names as the names.")
    }
    for (structure in names(vertices)) {
      parcel_vertices[[cifti.structure.short(structure)]] <- cifti.check.index.list(vertices[[structure]],
                                                                                   sprintf("vertices of parcel '%s'", name))
    }
  }
  voxels <- NULL
  if (!is.null(voxel_indices_ijk)) {
    voxel_indices_ijk <- as.integer(voxel_indices_ijk)
    if (length(voxel_indices_ijk) %% 3L != 0L || any(is.na(voxel_indices_ijk)) || any(voxel_indices_ijk < 0L)) {
      stop("Parameter 'voxel_indices_ijk' must contain non-negative, non-NA 0-based voxel indices, 3 per voxel.")
    }
    voxels <- matrix(voxel_indices_ijk, ncol = 3L, byrow = TRUE)
  }
  return(list(
    name = name,
    vertices = parcel_vertices,
    voxel_indices_ijk = voxels
  ))
}


#' @title Create a CIFTI-2 axis for parcels.
#'
#' @description A parcel axis describes the parcels of a parcellated matrix dimension (the
#'   mapping of `.pscalar`, `.ptseries`, `.pconn` and the second dimension of `.pdconn`).
#'   As for brain models, the index of a parcel is its position in the list.
#'
#' @param parcels list of parcels, as created by \code{\link{cifti.parcel}}.
#'
#' @param surfaces named integer vector or `NULL`, the number of vertices of the complete
#'   surfaces the parcels refer to, see \code{\link{cifti.axis.brain.models}}. Unlike
#'   Connectome Workbench, which writes them, this is optional here as well.
#'
#' @param volume a volume, see \code{\link{cifti.volume}}, for parcels that consist of
#'   volume voxels, or `NULL`.
#'
#' @return a named list with the entries 'type', 'parcels', 'surfaces' and 'volumes', an
#'   axis to be passed to \code{\link{cifti.header.from.axes}} or
#'   \code{\link{write.cifti}}.
#'
#' @examples
#' axis <- cifti.axis.parcels(list(
#'   cifti.parcel("PARCEL_A", list(CORTEX_LEFT = 0:2)),
#'   cifti.parcel("PARCEL_B", list(CORTEX_LEFT = 3:5))))
#' length(axis$parcels)
#'
#' @family cifti functions
#' @export
cifti.axis.parcels <- function(parcels, surfaces = NULL, volume = NULL) {
  if (!is.list(parcels) || length(parcels) == 0L) {
    stop("Parameter 'parcels' must be a list of parcels, see cifti.parcel().")
  }
  if (!is.null(parcels[["name"]])) {
    parcels <- list(parcels) # a single parcel was passed
  }
  parcels <- lapply(seq_along(parcels), function(parcel_idx) {
    parcel <- parcels[[parcel_idx]]
    parcel$index <- as.integer(parcel_idx - 1L)
    return(parcel)
  })
  return(list(
    type = "CIFTI_INDEX_TYPE_PARCELS",
    parcels = parcels,
    surfaces = cifti.axis.surfaces(surfaces),
    volumes = if (is.null(volume)) NULL else list(volume)
  ))
}


#' @title Create a CIFTI-2 axis for a series.
#'
#' @description A series axis describes a dimension that holds an ordered sequence of
#'   samples, usually the time points of a `.dtseries` or `.ptseries` file. The value of
#'   series index `i` (0-based) is `(start + i * step) * 10^exponent`, in the unit that
#'   `unit` names.
#'
#' @param number_of_series_points integer, the number of samples, which must match the
#'   size of that matrix dimension of the data.
#'
#' @param start numeric, the value of the first sample.
#'
#' @param step numeric, the difference between consecutive samples.
#'
#' @param exponent integer, the power of ten the values are in (e.g. -3 for milliseconds).
#'
#' @param unit character string, one of 'SECOND', 'HERTZ', 'METER' or 'RADIAN'.
#'
#' @return a named list with the entries 'type' and 'series', an axis to be passed to
#'   \code{\link{cifti.header.from.axes}} or \code{\link{write.cifti}}.
#'
#' @examples
#' axis <- cifti.axis.series(10L, start = 0, step = 0.72, unit = "SECOND")
#' axis$series$number_of_series_points
#'
#' @family cifti functions
#' @export
cifti.axis.series <- function(number_of_series_points, start = 0, step = 1, exponent = 0L, unit = "SECOND") {
  if (!is.numeric(number_of_series_points) || length(number_of_series_points) != 1L ||
      is.na(number_of_series_points) || number_of_series_points < 1L) {
    stop("Parameter 'number_of_series_points' must be a single positive integer.")
  }
  if (!is.numeric(start) || length(start) != 1L || is.na(start) ||
      !is.numeric(step) || length(step) != 1L || is.na(step)) {
    stop("Parameters 'start' and 'step' must be single numbers.")
  }
  units <- c("SECOND", "HERTZ", "METER", "RADIAN")
  if (!is.character(unit) || length(unit) != 1L || !(toupper(unit) %in% units)) {
    stop(sprintf("Parameter 'unit' must be one of %s.", paste(units, collapse = ", ")))
  }
  if (!is.numeric(exponent) || length(exponent) != 1L || is.na(exponent)) {
    stop("Parameter 'exponent' must be a single integer.")
  }
  return(list(
    type = "CIFTI_INDEX_TYPE_SERIES",
    series = list(
      number_of_series_points = as.integer(number_of_series_points),
      start = as.numeric(start),
      step = as.numeric(step),
      exponent = as.integer(exponent),
      unit = toupper(unit)
    )
  ))
}


#' @title Create a CIFTI-2 axis for scalar or label maps.
#'
#' @description A scalars axis holds a set of maps (e.g. the myelin map and the cortical
#'   thickness of a subject in one `.dscalar` file), a labels axis holds a set of label maps
#'   (one per parcellation in a `.dlabel` file). Both are described by their map names; a
#'   labels axis can additionally have a label table per map, which maps the integer label
#'   keys in the data to names and colors.
#'
#' @param names character vector or `NULL`, the map names. The number of maps is the number
#'   of names, so this has to be given (use empty strings for unnamed maps).
#'
#' @param metadata list of metadata lists or `NULL`, one per map. Each metadata list is a
#'   named list of character strings, as returned for a map by \code{\link{read.cifti.header}}.
#'   This is used for the palette information that Connectome Workbench stores per map.
#'
#' @return a named list with the entries 'type' and 'named_maps', an axis to be passed to
#'   \code{\link{cifti.header.from.axes}} or \code{\link{write.cifti}}.
#'
#' @examples
#' axis <- cifti.axis.scalars(c("thickness", "area"))
#' length(axis$named_maps)
#'
#' @family cifti functions
#' @export
cifti.axis.scalars <- function(names = NULL, metadata = NULL) {
  return(cifti.axis.named.maps(names = names, metadata = metadata, label_tables = NULL,
                               type = "CIFTI_INDEX_TYPE_SCALARS"))
}


#' @title Create a CIFTI-2 axis for label maps.
#'
#' @description See \code{\link{cifti.axis.scalars}}; a labels axis is the same thing with
#'   integer label keys instead of scalar values, and it stores the label table that
#'   defines the keys.
#'
#' @inheritParams cifti.axis.scalars
#'
#' @param label_tables list of label tables or `NULL`, one per map. Each label table is a
#'   data.frame with the columns 'key' (integer), 'red', 'green', 'blue', 'alpha' (numeric
#'   in the range 0 to 1) and 'label' (character string), i.e. the format that
#'   \code{\link{cifti.label.table}} returns. `NULL` entries are allowed for maps without a
#'   label table.
#'
#' @return a named list with the entries 'type' and 'named_maps', an axis to be passed to
#'   \code{\link{cifti.header.from.axes}} or \code{\link{write.cifti}}.
#'
#' @examples
#' label_table <- data.frame(key = 0:1, red = c(1, 0), green = c(1, 0.5), blue = c(1, 1),
#'                           alpha = c(0, 1), label = c("???", "PARCEL_A"))
#' axis <- cifti.axis.labels("parcellation", label_tables = list(label_table))
#' axis$named_maps[[1]]$labels$label
#'
#' @family cifti functions
#' @export
cifti.axis.labels <- function(names = NULL, label_tables = NULL, metadata = NULL) {
  return(cifti.axis.named.maps(names = names, metadata = metadata, label_tables = label_tables,
                               type = "CIFTI_INDEX_TYPE_LABELS"))
}


#' @title Create a CIFTI-2 axis for named maps (internal helper).
#'
#' @param names character vector, the map names, or `NULL` for maps without a name.
#'
#' @param metadata list of metadata lists, or `NULL`.
#'
#' @param label_tables list of label tables, or `NULL`.
#'
#' @param type character string, 'CIFTI_INDEX_TYPE_SCALARS' or 'CIFTI_INDEX_TYPE_LABELS'.
#'
#' @return a named list with the entries 'type' and 'named_maps'.
#'
#' @keywords internal
cifti.axis.named.maps <- function(names, metadata, label_tables, type) {
  if (is.null(names)) {
    stop("Parameter 'names' must be given: the number of maps in the dimension is the number of names.")
  }
  if (!is.character(names) || any(is.na(names))) {
    stop("Parameter 'names' must be a character vector.")
  }
  if (!is.null(metadata) && (!is.list(metadata) || length(metadata) != length(names))) {
    stop("Parameter 'metadata' must be a list with one entry per map.")
  }
  if (!is.null(label_tables) && (!is.list(label_tables) || length(label_tables) != length(names))) {
    stop("Parameter 'label_tables' must be a list with one entry per map.")
  }
  named_maps <- lapply(seq_along(names), function(map_idx) {
    return(list(
      name = names[map_idx],
      metadata = if (is.null(metadata)) NULL else metadata[[map_idx]],
      labels = if (is.null(label_tables)) NULL else cifti.check.label.table(label_tables[[map_idx]], map_idx)
    ))
  })
  return(list(type = type, named_maps = named_maps))
}


#' @title Create a CIFTI-2 axis from a template file.
#'
#' @description Read the axes of an existing CIFTI-2 file, so that a new file can be
#'   written with the same mapping. This is the recommended way to write a file for real
#'   data: the mapping of an HCP grayordinates file (which vertices are in the file, and
#'   which volume voxels) can not be invented, it has to come from the file the data was
#'   derived from.
#'
#' @param x character string (the path of a CIFTI-2 file), an `fs.cifti` object (see
#'   \code{\link{read.cifti.header}}) or an `fs.cifti.data` object (see
#'   \code{\link{read.cifti}}).
#'
#' @param dim integer or `NULL`, the matrix dimension to get the axis for. If `NULL` (the
#'   default), the axes of all matrix dimensions are returned as a list.
#'
#' @return the axis of the requested dimension (a named list, see
#'   \code{\link{cifti.axis.brain.models}}), or a list of axes (one per dimension; note
#'   that a file whose dimensions share a single XML mapping returns the same axis for
#'   both, as two entries).
#'
#' @examples
#' cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' axes <- cifti.axis.from.template(cifti_file)
#' length(axes)
#' axes[[2L]]$type
#'
#' @family cifti functions
#' @export
cifti.axis.from.template <- function(x, dim = NULL) {
  cii <- cifti.header.of(x)
  axes <- lapply(seq_along(cii$matrix$dim_sizes) - 1L, function(matrix_dim) {
    axis <- cifti.map.for.dim(cii, matrix_dim)
    axis$dims <- NULL # the dimension is given by the position in the list, not by the mapping
    return(axis)
  })
  if (is.null(dim)) {
    return(axes)
  }
  if (!is.numeric(dim) || length(dim) != 1L || is.na(dim) || dim < 0L ||
      dim > (length(axes) - 1L)) {
    stop(sprintf("Parameter 'dim' must be NULL or an integer in range 0 to %d.", length(axes) - 1L))
  }
  return(axes[[as.integer(dim) + 1L]])
}
