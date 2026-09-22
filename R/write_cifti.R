# The CIFTI-2 XML writer: build the XML document that describes a set of axes, and write
# a CIFTI-2 file (NIFTI-2 header + XML extension + data). See R/write_cifti_axes.R for the
# axis builders.


#' @title Create the CIFTI-2 XML metadata for a set of axes.
#'
#' @description Build the CIFTI-2 XML document (the part of a CIFTI-2 file that describes
#'   what the data matrix contains) from a set of axes. This is the inverse of the XML
#'   parsing that \code{\link{read.cifti.header}} performs: pass the result of this
#'   function to \code{\link{cifti.parse.xml}} or to \code{\link{read.cifti.header}} (after
#'   storing it in a file) and you get back the axes you put in.
#'
#' @param axes list of axes, one per matrix dimension, as created by the
#'   `cifti.axis.*()` functions or by \code{\link{cifti.axis.from.template}}. If the list
#'   is named, the names have to be the matrix dimensions ('0' and '1'), otherwise the axes
#'   are assigned to the dimensions in the order in which they are given.
#'
#' @param metadata named character vector or named list or `NULL`, the metadata of the
#'   matrix (e.g. the provenance information), as name/value pairs.
#'
#' @return character string, the CIFTI-2 XML document.
#'
#' @examples
#' axis_series <- cifti.axis.series(4L, start = 0, step = 2.5)
#' axis_brain <- cifti.axis.brain.models(list(
#'   cifti.brain.model.surface("lh", 10L),
#'   cifti.brain.model.surface("rh", 12L)))
#' xml <- cifti.header.from.axes(list(axis_series, axis_brain))
#' cat(substr(xml, 1, 200))
#'
#' @family cifti functions
#' @export
cifti.header.from.axes <- function(axes, metadata = NULL) {
  axes <- cifti.check.axes(axes)
  cifti.validate.axes(axes)

  root <- xml2::xml_new_root("CIFTI", "Version" = cifti.xml.version())
  matrix_node <- xml2::xml_add_child(root, "Matrix")
  cifti.xml.add.metadata(matrix_node, metadata)
  for (matrix_map in cifti.axes.to.maps(axes)) {
    cifti.xml.add.indices.map(matrix_node, matrix_map$axis, matrix_map$dims)
  }
  return(as.character(root))
}


#' @title The CIFTI version this package writes.
#'
#' @return character string, '2'.
#'
#' @keywords internal
cifti.xml.version <- function() {
  return("2")
}


# --- Writing whole CIFTI-2 files ---------------------------------------------

#' @title Write a CIFTI-2 file.
#'
#' @description Write a data matrix and its axes to a CIFTI-2 file, i.e. a NIFTI-2 file
#'   with the CIFTI-2 XML in a header extension. The axes describe what the matrix
#'   dimensions contain; they can be given explicitly (see the `cifti.axis.*()` functions),
#'   taken from a template file (recommended for real data, see
#'   \code{\link{cifti.axis.from.template}}), or both.
#'
#'   The file type (and thus the NIFTI intent code that identifies it) follows from the
#'   axes: a scalars dimension and a brain model dimension make a `.dscalar` file, a series
#'   dimension and a brain model dimension a `.dtseries` file, and so on. If the file name
#'   has one of the standard CIFTI extensions, the axes have to match it: a file named
#'   `.pdconn.nii` whose dimensions are not (brain models, parcels) is an error, not a
#'   warning, because the file name is the only hint that other software has about the
#'   content.
#'
#' @param filepath character string, the path of the file to write.
#'
#' @param data numeric or integer matrix or array, the data. The dimensions have to match
#'   the sizes of the axes, and the order is the same as for \code{\link{read.cifti}}: the
#'   first array dimension is CIFTI matrix dimension 0. A plain vector is interpreted as an
#'   array of that length (which is what you want for a file with a single map, e.g. a
#'   `.dscalar` with one map).
#'
#' @param axes list of axes, one per matrix dimension, or `NULL` (in which case `template`
#'   has to be given). See \code{\link{cifti.header.from.axes}} for the naming rules.
#'
#' @param template character string (the path of a CIFTI-2 file), an `fs.cifti` or an
#'   `fs.cifti.data` object or `NULL`. The axes of this file are used for the dimensions
#'   that `axes` does not describe, and also for the metadata if `metadata` is `NULL`.
#'
#' @param metadata named character vector or named list, or `NULL`, the matrix metadata to
#'   write. The default writes the metadata of the template file, if there is one, so that
#'   the provenance of the source data is kept.
#'
#' @return the axes that were written, invisibly. The file is written as a side effect.
#'
#' @note The data are written as 32 bit floating point values, which is what the reference
#'   implementations write for CIFTI-2 files (including for label files, whose keys are
#'   small integers). Values that need more than about 7 significant digits are rounded.
#'
#' @examples
#' axis_brain <- cifti.axis.brain.models(list(
#'   cifti.brain.model.surface("lh", 10L),
#'   cifti.brain.model.surface("rh", 12L)))
#' data_matrix <- matrix(1:22, nrow = 1L)
#' out_file <- file.path(tempdir(), "tiny_written.dscalar.nii")
#' write.cifti(out_file, data_matrix, axes = list(cifti.axis.scalars("my data"), axis_brain))
#' \dontshow{unlink(out_file)}
#' \dontrun{
#' # Write data with the mapping of an existing file (recommended for real data):
#' template <- "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii"
#' data_matrix <- read.cifti(template)$data
#' write.cifti("copy.dscalar.nii", data_matrix, template = template)
#' }
#'
#' @family cifti functions
#' @export
write.cifti <- function(filepath, data, axes = NULL, template = NULL, metadata = NULL) {
  if (!is.character(filepath) || length(filepath) != 1L || is.na(filepath)) {
    stop("Parameter 'filepath' must be a character string.")
  }
  if (guess.filename.is.gzipped(filepath, gz_extensions = c(".gz"))) {
    stop(sprintf(paste0("CIFTI-2 files must not be compressed, but the file name '%s' suggests a gzipped file. The ",
                        "CIFTI-2 format forbids compression so that random access to the data remains possible.\n"), filepath))
  }

  template_cii <- NULL
  if (!is.null(template)) {
    template_cii <- cifti.header.of(template)
  }

  if (is.null(axes) && is.null(template_cii)) {
    stop("Either 'axes' or 'template' has to be given: a CIFTI-2 file states which brainordinates, parcels, maps or series its matrix dimensions contain, and that information cannot be derived from the data alone.")
  }

  axes <- cifti.merge.axes(axes, template_cii)
  cifti.validate.axes(axes)

  if (is.null(metadata) && !is.null(template_cii)) {
    metadata <- template_cii$matrix$metadata
  }

  data <- cifti.check.data.for.axes(data, axes)
  file_type <- cifti.file.type.for.axes(axes)
  cifti.validate.file.extension(filepath, file_type)
  if (!nzchar(file_type$extension[1L])) {
    warning(sprintf(paste0("The axes describe the matrix index types (%s, %s), which is not one of the nine standard CIFTI-2 ",
                           "file types, so the file gets the intent code 3000 ('ConnUnknown'). Other software may refuse to read it.",
                           "\n"),
                    sub("^CIFTI_INDEX_TYPE_", "", file_type$dim0_type[1L]),
                    sub("^CIFTI_INDEX_TYPE_", "", file_type$dim1_type[1L])))
  }

  xml <- cifti.header.from.axes(axes, metadata = metadata)
  niiheader <- cifti.nifti.header.for.axes(axes, intent_code = file_type$intent_code,
                                           intent_name = file_type$intent_name)
  write.nifti2(filepath, data, niiheader,
               extensions = list(nifti2.extension(CIFTI_EXTENSION_CODE, xml)))
  return(invisible(axes))
}


#' @title Get the standard CIFTI-2 file type for a set of axes.
#'
#' @description Look up the file type that the format defines for the combination of matrix
#'   index types of the axes, see \code{\link{cifti.file.types}}.
#'
#' @param axes list of axes, see \code{\link{cifti.header.from.axes}}.
#'
#' @return a one row data.frame, see \code{\link{cifti.file.types}}, or a row with
#'   `intent_code = 3000` ('ConnUnknown') and empty file name extension for a combination
#'   that the standard does not define.
#'
#' @examples
#' axis_brain <- cifti.axis.brain.models(list(cifti.brain.model.surface("lh", 10L)))
#' type <- cifti.file.type.for.axes(list(cifti.axis.scalars("m"), axis_brain))
#' type$extension
#'
#' @family cifti functions
#' @export
cifti.file.type.for.axes <- function(axes) {
  axes <- cifti.check.axes(axes)
  types <- unname(vapply(axes, function(axis) axis$type, character(1L)))
  file_types <- cifti.file.types()
  if (length(types) == 2L) {
    for (row_idx in seq_len(nrow(file_types))) {
      if (identical(file_types$dim0_type[row_idx], types[1L]) &&
          identical(file_types$dim1_type[row_idx], types[2L])) {
        return(file_types[row_idx, , drop = FALSE])
      }
    }
  }
  return(data.frame(extension = "", intent_code = 3000L, intent_name = "ConnUnknown",
                    dim0_type = types[1L], dim1_type = if (length(types) > 1L) types[2L] else NA_character_,
                    stringsAsFactors = FALSE))
}


#' @title Check the file extension against the axes of the data.
#'
#' @description A CIFTI-2 file name states the file type (`.dscalar`, `.pdconn`, ...), and
#'   the type decides which matrix index types the two dimensions have. A file whose name
#'   contradicts its content is silently misread by other software, so this is an error.
#'
#' @param filepath character string, the file name to write.
#'
#' @param file_type a one row data.frame, see \code{\link{cifti.file.type.for.axes}}.
#'
#' @return `NULL`, invisibly. Stops if the file name names a different file type.
#'
#' @keywords internal
cifti.validate.file.extension <- function(filepath, file_type) {
  file_types <- cifti.file.types()
  expected <- NA_character_
  for (row_idx in seq_len(nrow(file_types))) {
    if (endsWith(filepath, file_types$extension[row_idx])) {
      expected <- file_types$extension[row_idx]
      break
    }
  }
  if (is.na(expected)) {
    return(invisible(NULL))
  }
  if (!identical(expected, file_type$extension)) {
    short_type <- function(type) {
      return(sub("^CIFTI_INDEX_TYPE_", "", type))
    }
    stop(sprintf(paste0("The file name '%s' names the CIFTI-2 file type '%s', which means that the two matrix dimensions are ",
                        "(%s, %s), but the axes of the data describe (%s, %s), which is the file type '%s'. Rename the file or ",
                        "fix the axes: the file name is the only hint about the content that other software gets before ",
                        "reading the file.\n"),
                 basename(filepath), expected,
                 short_type(file_types$dim0_type[file_types$extension == expected]),
                 short_type(file_types$dim1_type[file_types$extension == expected]),
                 short_type(file_type$dim0_type[1L]), short_type(file_type$dim1_type[1L]),
                 if (nzchar(file_type$extension[1L])) file_type$extension[1L] else "not a standard one"))
  }
  return(invisible(NULL))
}


#' @title Build the NIFTI-2 header of a CIFTI-2 file.
#'
#' @description CIFTI-2 files are NIFTI-2 files whose header has a fixed shape: the matrix
#'   dimensions are stored in the `dim` field (dimension 0 in `dim[5]`, dimension 1 in
#'   `dim[6]`, which is why `dim[0]` is 6 for a two-dimensional matrix), the intent code
#'   names the file type, the voxel sizes, the scaling and the geometry fields are unused
#'   (the geometry of a CIFTI-2 file is in its XML), and the data start after the header
#'   extension that holds the XML.
#'
#' @param axes list of axes, see \code{\link{cifti.header.from.axes}}.
#'
#' @param intent_code integer, the NIFTI intent code of the file type, see
#'   \code{\link{cifti.file.types}}.
#'
#' @param intent_name character string, the NIFTI intent name of the file type.
#'
#' @return a named list, a NIFTI-2 header as returned by \code{\link{read.nifti2.header}}.
#'
#' @keywords internal
cifti.nifti.header.for.axes <- function(axes, intent_code, intent_name) {
  axes <- cifti.check.axes(axes)
  dim_sizes <- vapply(axes, cifti.axis.size, integer(1L))
  niiheader <- ni2header.template()

  # The matrix dimensions are dim[5], dim[6], ... of the NIFTI-2 header. The values before
  # them are 1, and dim[0] is the number of used dimensions, which is 4 + the number of
  # matrix dimensions (6 for the two-dimensional matrix of every standard file type).
  niiheader$dim <- c(4L + length(dim_sizes), rep(1L, 4L), dim_sizes, rep(1L, 3L - length(dim_sizes)))
  niiheader$dim <- niiheader$dim[1:8]
  niiheader$intent_code <- as.integer(intent_code)
  niiheader$intent_name <- as.character(intent_name)
  niiheader$datatype <- 16L # float32, which is what the reference implementations write
  niiheader$bitpix <- 32L
  niiheader$scl_slope <- 1.0
  niiheader$scl_inter <- 0.0
  niiheader$cal_min <- 0.0
  niiheader$cal_max <- 0.0
  niiheader$xyzt_units <- 10L # mm and seconds, like Connectome Workbench writes it
  niiheader$descrip <- "CIFTI-2 file written by freesurferformats"
  return(niiheader)
}


# --- Internal helpers --------------------------------------------------------

#' @title Accept the input forms of a CIFTI-2 header.
#'
#' @param x character string (a file path), an `fs.cifti` or an `fs.cifti.data` object.
#'
#' @return an `fs.cifti` object, see \code{\link{read.cifti.header}}.
#'
#' @keywords internal
cifti.header.of <- function(x) {
  if (inherits(x, "fs.cifti")) {
    return(x)
  }
  if (inherits(x, "fs.cifti.data")) {
    return(x$header)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(read.cifti.header(x))
  }
  stop("Parameter 'x' must be an fs.cifti object (see read.cifti.header()), an fs.cifti.data object (see read.cifti()) or the path of a CIFTI-2 file.")
}


#' @title Check a list of axes and name the dimensions.
#'
#' @param axes list of axes, see \code{\link{cifti.header.from.axes}}.
#'
#' @return the list, with the axes named by their matrix dimension.
#'
#' @keywords internal
cifti.check.axes <- function(axes) {
  if (!is.list(axes) || length(axes) == 0L) {
    stop("Parameter 'axes' must be a list of one or two axes, see cifti.axis.brain.models() and friends.")
  }
  axis_names <- names(axes)
  if (is.null(axis_names) || any(!nzchar(axis_names)) || any(is.na(suppressWarnings(as.integer(axis_names))))) {
    # Unnamed axes (or names that are not matrix dimensions) are assigned to the matrix
    # dimensions in the order in which they are given.
    names(axes) <- as.character(seq_along(axes) - 1L)
  } else {
    dims <- as.integer(axis_names)
    if (any(dims < 0L) || anyDuplicated(dims)) {
      stop("The axis names must be distinct matrix dimensions, e.g. '0' and '1'.")
    }
    names(axes) <- as.character(dims)
  }
  if (length(axes) > 2L) {
    stop(sprintf(paste0("A CIFTI-2 file has at most two matrix dimensions, but %d axes were given. (The format allows ",
                        "more in principle, but no reader or writer of the reference implementations supports it, and this ",
                        "package does not support it either.)\n"), length(axes)))
  }
  axes <- axes[order(as.integer(names(axes)))]
  for (axis_idx in seq_along(axes)) {
    if (!is.list(axes[[axis_idx]]) || is.null(axes[[axis_idx]]$type)) {
      stop("Each axis must be a named list with a 'type' entry, see cifti.axis.brain.models() and friends.")
    }
    if (!(axes[[axis_idx]]$type %in% cifti.index.types())) {
      stop(sprintf("The axis for matrix dimension %s has the unknown type '%s'. Supported types are: %s.\n",
                   names(axes)[axis_idx], axes[[axis_idx]]$type, paste(cifti.index.types(), collapse = ", ")))
    }
  }
  return(axes)
}


#' @title Validate the axes of a CIFTI-2 file.
#'
#' @description Check the things about a set of axes that make a file invalid rather than
#'   just unusual: the sizes of the axes, the index ranges of the brain models, the
#'   structures a volume model or a parcel refers to, and the label keys of a label table.
#'
#' @param axes list of axes, see \code{\link{cifti.header.from.axes}}.
#'
#' @return `NULL`, invisibly. Stops with a descriptive error.
#'
#' @keywords internal
cifti.validate.axes <- function(axes) {
  axes <- cifti.check.axes(axes)
  for (axis_idx in seq_along(axes)) {
    axis <- axes[[axis_idx]]
    dim_label <- sprintf("matrix dimension %s", names(axes)[axis_idx])
    if (axis$type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
      if (is.null(axis$brain_models) || length(axis$brain_models) == 0L) {
        stop(sprintf("The axis for %s has no brain model entries.\n", dim_label))
      }
      # The same checks that the reader applies to a file it reads.
      cifti.validate.brain.models(axis, dim_size = cifti.axis.size(axis), filepath = "the file to be written")
    } else if (axis$type == "CIFTI_INDEX_TYPE_PARCELS") {
      if (is.null(axis$parcels) || length(axis$parcels) == 0L) {
        stop(sprintf("The axis for %s has no parcels.\n", dim_label))
      }
      # Note that the parcel vertex indices are checked against the surfaces the axis
      # declares, where it declares any: Connectome Workbench writes no Surface elements,
      # so a file without them is complete, not inconsistent.
      declared_surfaces <- cifti.surface.vertex.counts(axis)
      for (parcel in axis$parcels) {
        if (is.null(parcel$vertices) && is.null(parcel$voxel_indices_ijk)) {
          stop(sprintf("Parcel '%s' of %s is empty: it has neither surface vertices nor volume voxels.\n",
                       parcel$name, dim_label))
        }
        if (!is.null(parcel$voxel_indices_ijk) && is.null(axis$volumes)) {
          stop(sprintf(paste0("Parcel '%s' of %s contains volume voxels, but the axis has no volume: the voxel indices ",
                              "cannot be interpreted without it, see cifti.volume().\n"), parcel$name, dim_label))
        }
        for (structure_short in names(parcel$vertices)) {
          if (structure_short %in% names(declared_surfaces)) {
            cifti.validate.index.range(parcel$vertices[[structure_short]],
                                       max_index = as.integer(declared_surfaces[[structure_short]]) - 1L,
                                       what = sprintf("vertex indices of parcel '%s'", parcel$name))
          }
        }
      }
    } else if (axis$type == "CIFTI_INDEX_TYPE_SERIES") {
      if (is.null(axis$series) || is.null(axis$series$number_of_series_points)) {
        stop(sprintf("The axis for %s is a series without the number of series points.\n", dim_label))
      }
    } else if (axis$type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
      if (is.null(axis$named_maps) || length(axis$named_maps) == 0L) {
        stop(sprintf("The axis for %s has no named maps.\n", dim_label))
      }
      if (identical(axis$type, "CIFTI_INDEX_TYPE_LABELS")) {
        for (map_idx in seq_along(axis$named_maps)) {
          if (!is.null(axis$named_maps[[map_idx]]$labels)) {
            cifti.check.label.table(axis$named_maps[[map_idx]]$labels, map_idx)
          }
        }
      }
    }
  }
  return(invisible(NULL))
}


#' @title Check a label table for writing.
#'
#' @param label_table a data.frame, see \code{\link{cifti.axis.labels}}, or `NULL`.
#'
#' @param map_idx integer, the number of the map, used in the error message.
#'
#' @return the label table, or `NULL`.
#'
#' @keywords internal
cifti.check.label.table <- function(label_table, map_idx) {
  if (is.null(label_table)) {
    return(NULL)
  }
  if (!is.data.frame(label_table)) {
    stop(sprintf("The label table of map %d must be a data.frame, see cifti.axis.labels().", as.integer(map_idx)))
  }
  required <- c("key", "red", "green", "blue", "alpha", "label")
  missing_columns <- setdiff(required, names(label_table))
  if (length(missing_columns) > 0L) {
    stop(sprintf("The label table of map %d is missing the columns: %s.\n",
                 as.integer(map_idx), paste(missing_columns, collapse = ", ")))
  }
  if (anyDuplicated(label_table$key)) {
    stop(sprintf("The label table of map %d contains duplicate label keys, which the format does not allow.\n",
                 as.integer(map_idx)))
  }
  values <- as.matrix(label_table[, c("red", "green", "blue", "alpha")])
  if (any(is.na(values)) || any(values < 0.) || any(values > 1.)) {
    stop(sprintf(paste0("The colors in the label table of map %d must be in the range 0 to 1 (which is how the format ",
                        "stores them, not the 0 to 255 of a FreeSurfer color lookup table).\n"), as.integer(map_idx)))
  }
  if (any(is.na(label_table$key)) || any(label_table$key < 0L)) {
    stop(sprintf("The label keys of map %d must be non-negative, non-NA integers.\n", as.integer(map_idx)))
  }
  label_table$key <- as.integer(label_table$key)
  return(label_table)
}


#' @title The number of matrix entries an axis covers.
#'
#' @param axis an axis, see \code{\link{cifti.header.from.axes}}.
#'
#' @return integer, the size of that matrix dimension.
#'
#' @keywords internal
cifti.axis.size <- function(axis) {
  if (!is.list(axis) || is.null(axis$type)) {
    stop("Parameter 'axis' must be an axis, see cifti.axis.brain.models() and friends.")
  }
  if (axis$type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    return(as.integer(sum(vapply(axis$brain_models, function(model) {
      return(as.integer(model$index_count))
    }, integer(1L)))))
  }
  if (axis$type == "CIFTI_INDEX_TYPE_PARCELS") {
    return(as.integer(length(axis$parcels)))
  }
  if (axis$type == "CIFTI_INDEX_TYPE_SERIES") {
    return(as.integer(axis$series$number_of_series_points))
  }
  if (axis$type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
    return(as.integer(length(axis$named_maps)))
  }
  stop(sprintf("Unsupported axis type '%s'.\n", axis$type)) # nocov
}


#' @title Compute the index ranges of brain model entries.
#'
#' @description The `IndexOffset` and `IndexCount` fields of a brain model entry say which
#'   matrix entries it covers. They are derived from the index lists (and from the surface
#'   sizes or the volume for a model that covers everything), and they have to cover the
#'   dimension without gaps, so they are always computed here rather than taken from the
#'   caller.
#'
#' @param models list of brain model entries, see \code{\link{cifti.brain.model.surface}}.
#'
#' @param volume a volume or `NULL`, see \code{\link{cifti.volume}}. Needed for a volume
#'   model that has no voxel index list (which covers all voxels of the volume).
#'
#' @return the models, with `index_offset` and `index_count` set.
#'
#' @keywords internal
cifti.compute.index.ranges <- function(models, volume = NULL) {
  offset <- 0L
  for (model_idx in seq_along(models)) {
    model <- models[[model_idx]]
    if (is.null(model$model_type) || !(model$model_type %in% cifti.model.types())) {
      stop(sprintf("Brain model %d has the invalid model type '%s'. Supported are: %s.\n",
                   model_idx - 1L, as.character(model$model_type), paste(cifti.model.types(), collapse = ", ")))
    }
    if (is.null(model$brain_structure) || is.na(model$brain_structure)) {
      stop(sprintf("Brain model %d has no brain structure.\n", model_idx - 1L))
    }
    model$brain_structure <- cifti.structure.canonical(model$brain_structure)

    if (identical(model$model_type, "CIFTI_MODEL_TYPE_SURFACE")) {
      if (is.null(model$surface_number_of_vertices) || is.na(model$surface_number_of_vertices)) {
        stop(sprintf(paste0("The surface brain model for structure '%s' has no number of vertices of the complete surface. ",
                            "The CIFTI-2 format requires it, see cifti.brain.model.surface().\n"), model$brain_structure))
      }
      if (is.null(model$vertex_indices)) {
        index_count <- as.integer(model$surface_number_of_vertices)
      } else {
        index_count <- length(model$vertex_indices)
      }
    } else {
      if (is.null(model$voxel_indices_ijk)) {
        if (is.null(volume)) {
          stop(sprintf(paste0("The volume brain model for structure '%s' has no voxel index list, which means that it covers ",
                              "all voxels of the volume. Without the volume there is no way to know how many voxels that is, ",
                              "see cifti.volume().\n"), model$brain_structure))
        }
        index_count <- as.integer(prod(volume$dimensions))
      } else {
        index_count <- nrow(model$voxel_indices_ijk)
      }
    }
    model$index_offset <- offset
    model$index_count <- index_count
    models[[model_idx]] <- model
    if (!is.na(index_count)) {
      offset <- offset + index_count
    }
  }
  return(models)
}


#' @title Check and prepare the data for a set of axes.
#'
#' @param data the data, see \code{\link{write.cifti}}.
#'
#' @param axes list of axes, see \code{\link{cifti.header.from.axes}}.
#'
#' @return the data as an array with the dimensions of the axes.
#'
#' @keywords internal
cifti.check.data.for.axes <- function(data, axes) {
  if (!is.numeric(data) && !is.integer(data)) {
    stop("Parameter 'data' must be numeric or integer.")
  }
  dim_sizes <- vapply(axes, cifti.axis.size, integer(1L))
  if (is.null(dim(data))) {
    if (length(data) != prod(dim_sizes)) {
      stop(sprintf(paste0("The data have %d values, but the axes describe a matrix of size %s (%d values).\n"),
                   length(data), paste(dim_sizes, collapse = " x "), prod(dim_sizes)))
    }
    data <- array(as.numeric(data), dim = dim_sizes)
  }
  if (length(dim(data)) != length(dim_sizes) || !identical(as.integer(dim(data)), as.integer(dim_sizes))) {
    stop(sprintf("The data have the dimensions %s, but the axes describe a matrix of size %s.\n",
                 paste(dim(data), collapse = " x "), paste(dim_sizes, collapse = " x ")))
  }
  return(array(as.numeric(data), dim = dim_sizes))
}


#' @title Merge explicitly given axes with the axes of a template.
#'
#' @param axes list of axes or `NULL`, see \code{\link{write.cifti}}.
#'
#' @param template_cii an `fs.cifti` object or `NULL`.
#'
#' @return the merged list of axes, named by matrix dimension.
#'
#' @keywords internal
cifti.merge.axes <- function(axes, template_cii) {
  if (is.null(template_cii)) {
    return(cifti.check.axes(axes))
  }
  template_axes <- cifti.axis.from.template(template_cii)
  names(template_axes) <- as.character(seq_along(template_axes) - 1L)
  if (is.null(axes)) {
    return(template_axes)
  }
  axes <- cifti.check.axes(axes)
  for (axis_dim in names(axes)) {
    if (!(axis_dim %in% names(template_axes))) {
      stop(sprintf(paste0("Axis for matrix dimension %s was given, but the template file has only %d matrix dimensions. ",
                          "The number of dimensions and the mapping of the dimensions that are not replaced come from the ",
                          "template.\n"), axis_dim, length(template_axes)))
    }
    template_axes[[axis_dim]] <- axes[[axis_dim]]
  }
  return(template_axes)
}


#' @title Group axes into MatrixIndicesMap elements.
#'
#' @description A CIFTI-2 file describes its matrix dimensions with `MatrixIndicesMap`
#'   elements, and the format says that the same element must be used for both dimensions
#'   when they describe the same thing. This is the case for connectome files (a `.dconn`
#'   or `.pconn` has the same brainordinates or parcels in both dimensions), and Connectome
#'   Workbench writes such a file with a single element that applies to both dimensions,
#'   while nibabel writes one per dimension. The merged form is what this writer produces.
#'
#' @param axes list of axes, named by matrix dimension.
#'
#' @return a list of lists with the entries 'axis' (the axis) and 'dims' (the matrix
#'   dimensions it describes).
#'
#' @keywords internal
cifti.axes.to.maps <- function(axes) {
  if (length(axes) == 2L && identical(axes[[1L]], axes[[2L]])) {
    return(list(list(axis = axes[[1L]], dims = c(0L, 1L))))
  }
  return(lapply(seq_along(axes), function(axis_idx) {
    return(list(axis = axes[[axis_idx]], dims = as.integer(names(axes)[axis_idx])))
  }))
}


# --- The XML elements --------------------------------------------------------

#' @title Add a MetaData element to an XML node.
#'
#' @param node an xml2 node, the parent element.
#'
#' @param metadata named character vector, named list or `NULL`, the name/value pairs.
#'
#' @return `NULL`, invisibly. The element is added as a child of `node`.
#'
#' @keywords internal
cifti.xml.add.metadata <- function(node, metadata) {
  if (is.null(metadata) || length(metadata) == 0L) {
    return(invisible(NULL))
  }
  if (is.list(metadata) && !is.null(names(metadata))) {
    metadata <- unlist(metadata)
  }
  if (!is.character(metadata) || is.null(names(metadata))) {
    stop("Parameter 'metadata' must be a named character vector (or a named list of character strings) with the metadata name/value pairs.")
  }
  metadata_node <- xml2::xml_add_child(node, "MetaData")
  for (entry_idx in seq_along(metadata)) {
    md_node <- xml2::xml_add_child(metadata_node, "MD")
    xml2::xml_add_child(md_node, "Name", names(metadata)[entry_idx])
    xml2::xml_add_child(md_node, "Value", as.character(metadata[entry_idx]))
  }
  return(invisible(NULL))
}


#' @title Add a MatrixIndicesMap element to the Matrix element.
#'
#' @param matrix_node an xml2 node, the `Matrix` element.
#'
#' @param axis the axis to write, see \code{\link{cifti.header.from.axes}}.
#'
#' @param dims integer vector, the matrix dimensions the element applies to.
#'
#' @return `NULL`, invisibly. The element is added as a child of `matrix_node`.
#'
#' @keywords internal
cifti.xml.add.indices.map <- function(matrix_node, axis, dims) {
  attributes <- list("AppliesToMatrixDimension" = paste(as.integer(dims), collapse = ","),
                     "IndicesMapToDataType" = axis$type)
  if (identical(axis$type, "CIFTI_INDEX_TYPE_SERIES")) {
    series <- axis$series
    attributes$NumberOfSeriesPoints <- as.character(as.integer(series$number_of_series_points))
    attributes$SeriesStart <- cifti.xml.num(series$start)
    attributes$SeriesStep <- cifti.xml.num(series$step)
    attributes$SeriesExponent <- as.character(as.integer(series$exponent))
    attributes$SeriesUnit <- series$unit
  }
  map_node <- do.call(xml2::xml_add_child, c(list(matrix_node, "MatrixIndicesMap"), attributes))

  for (surface in axis$surfaces) {
    xml2::xml_add_child(map_node, "Surface", BrainStructure = surface$brain_structure,
                        SurfaceNumberOfVertices = as.character(as.integer(surface$surface_number_of_vertices)))
  }
  for (volume in axis$volumes) {
    volume_node <- xml2::xml_add_child(map_node, "Volume",
                                       VolumeDimensions = paste(as.integer(volume$dimensions), collapse = ","))
    matrix_node_child <- xml2::xml_add_child(volume_node, "TransformationMatrixVoxelIndicesIJKtoXYZ",
                                             MeterExponent = as.character(as.integer(volume$meter_exponent)))
    xml2::xml_set_text(matrix_node_child, cifti.xml.num.vector(as.vector(t(volume$transformation_matrix)), decimals = 10L))
  }

  if (identical(axis$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")) {
    for (model in axis$brain_models) {
      model_node <- xml2::xml_add_child(map_node, "BrainModel",
                                        IndexOffset = as.character(as.integer(model$index_offset)),
                                        IndexCount = as.character(as.integer(model$index_count)),
                                        BrainStructure = model$brain_structure,
                                        ModelType = model$model_type)
      if (identical(model$model_type, "CIFTI_MODEL_TYPE_SURFACE")) {
        xml2::xml_set_attr(model_node, "SurfaceNumberOfVertices", as.character(as.integer(model$surface_number_of_vertices)))
        child <- xml2::xml_add_child(model_node, "VertexIndices")
        # A model without an index list covers everything, but the list is still written
        # out: Connectome Workbench refuses a BrainModel element without a child element
        # ('BrainModel requires a child element'), so the explicit form is the only one
        # that every implementation accepts.
        indices <- if (is.null(model$vertex_indices)) {
          seq.int(0L, length.out = as.integer(model$surface_number_of_vertices))
        } else {
          model$vertex_indices
        }
        xml2::xml_set_text(child, cifti.xml.int.vector(indices))
      } else {
        child <- xml2::xml_add_child(model_node, "VoxelIndicesIJK")
        voxels <- model$voxel_indices_ijk
        if (is.null(voxels)) {
          volume <- axis$volumes[[1L]]
          voxels <- as.matrix(expand.grid(i = seq.int(0L, volume$dimensions[1L] - 1L),
                                          j = seq.int(0L, volume$dimensions[2L] - 1L),
                                          k = seq.int(0L, volume$dimensions[3L] - 1L)))
        }
        # Connectome Workbench writes one voxel per line, nibabel all of them on a single
        # line; both are accepted by every implementation, and the line based layout is
        # much easier to read and to diff for a human.
        xml2::xml_set_text(child, paste0(apply(voxels, 1L, function(voxel) {
          return(paste(as.integer(voxel), collapse = " "))
        }), collapse = "\n"))
      }
    }
  } else if (identical(axis$type, "CIFTI_INDEX_TYPE_PARCELS")) {
    for (parcel in axis$parcels) {
      parcel_node <- xml2::xml_add_child(map_node, "Parcel", Name = parcel$name)
      for (structure_short in names(parcel$vertices)) {
        child <- xml2::xml_add_child(parcel_node, "Vertices",
                                     BrainStructure = cifti.structure.canonical(structure_short))
        xml2::xml_set_text(child, cifti.xml.int.vector(parcel$vertices[[structure_short]]))
      }
      if (!is.null(parcel$voxel_indices_ijk)) {
        child <- xml2::xml_add_child(parcel_node, "VoxelIndicesIJK")
        xml2::xml_set_text(child, paste0(apply(parcel$voxel_indices_ijk, 1L, function(voxel) {
          return(paste(as.integer(voxel), collapse = " "))
        }), collapse = "\n"))
      }
    }
  } else if (axis$type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
    for (named_map in axis$named_maps) {
      named_map_node <- xml2::xml_add_child(map_node, "NamedMap")
      xml2::xml_add_child(named_map_node, "MapName", as.character(named_map$name))
      cifti.xml.add.metadata(named_map_node, named_map$metadata)
      if (!is.null(named_map$labels)) {
        label_table_node <- xml2::xml_add_child(named_map_node, "LabelTable")
        for (label_idx in seq_len(nrow(named_map$labels))) {
          label <- named_map$labels[label_idx, ]
          label_node <- xml2::xml_add_child(label_table_node, "Label",
                                            Key = as.character(as.integer(label$key)),
                                            Red = cifti.xml.num(label$red),
                                            Green = cifti.xml.num(label$green),
                                            Blue = cifti.xml.num(label$blue),
                                            Alpha = cifti.xml.num(label$alpha))
          xml2::xml_set_text(label_node, as.character(label$label))
        }
      }
    }
  }
  return(invisible(NULL))
}


#' @title Format a number like the reference implementations do.
#'
#' @description Connectome Workbench writes fixed point values with 10 decimal places in
#'   the XML (e.g. `SeriesStep="2.5000000000"`), and this keeps the output comparable with
#'   the files it writes. Colors of a label table are written with the precision the file
#'   had, which is 3 decimal places in the official example files.
#'
#' @param x numeric, the value.
#'
#' @param decimals integer, the number of decimal places.
#'
#' @return character string.
#'
#' @keywords internal
cifti.xml.num <- function(x, decimals = 10L) {
  return(sprintf(paste0("%.", as.integer(decimals), "f"), as.numeric(x)))
}


#' @title Format a numeric vector for an XML text node.
#'
#' @param x numeric vector.
#'
#' @param decimals integer, the number of decimal places.
#'
#' @return character string, the values separated by single spaces.
#'
#' @keywords internal
cifti.xml.num.vector <- function(x, decimals = 10L) {
  return(paste(sprintf(paste0("%.", as.integer(decimals), "f"), as.numeric(x)), collapse = " "))
}


#' @title Format an integer vector for an XML text node.
#'
#' @param x integer vector.
#'
#' @return character string, the values separated by single spaces.
#'
#' @keywords internal
cifti.xml.int.vector <- function(x) {
  return(paste(as.integer(x), collapse = " "))
}


#' @title Check a vector of 0-based indices.
#'
#' @param indices integer vector, the indices.
#'
#' @param what character string, a description of the indices, used in error messages.
#'
#' @param max_index integer or `NULL`, the largest allowed index.
#'
#' @return the indices as an integer vector.
#'
#' @keywords internal
cifti.check.index.list <- function(indices, what, max_index = NULL) {
  indices <- as.integer(indices)
  if (length(indices) == 0L) {
    return(indices)
  }
  if (any(is.na(indices)) || any(indices < 0L)) {
    stop(sprintf("The %s must be non-negative, non-NA 0-based indices, but found %s.\n",
                 what, paste(utils::head(indices[is.na(indices) | indices < 0L], 10L), collapse = ", ")))
  }
  if (!is.null(max_index) && any(indices > max_index)) {
    stop(sprintf("The %s contain indices above %d (they must be 0-based and refer to an existing vertex or voxel), but found %s.\n",
                 what, max_index, paste(utils::head(indices[indices > max_index], 10L), collapse = ", ")))
  }
  if (anyDuplicated(indices)) {
    stop(sprintf(paste0("The %s contain duplicate indices. A CIFTI-2 matrix has exactly one entry per brainordinate, so a ",
                        "vertex or voxel cannot appear twice in the same matrix dimension.\n"), what))
  }
  return(indices)
}


#' @title Create the Surface elements of an axis.
#'
#' @param surfaces named integer vector or `NULL`, the number of vertices per brain
#'   structure, see \code{\link{cifti.axis.brain.models}}.
#'
#' @return a list of lists with the entries 'brain_structure' and
#'   'surface_number_of_vertices', or `NULL`.
#'
#' @keywords internal
cifti.axis.surfaces <- function(surfaces) {
  if (is.null(surfaces)) {
    return(NULL)
  }
  if (!is.numeric(surfaces) || is.null(names(surfaces)) || any(is.na(surfaces))) {
    stop("Parameter 'surfaces' must be a named integer vector with the number of vertices per brain structure (e.g. c(CORTEX_LEFT = 32492L)).")
  }
  return(lapply(seq_along(surfaces), function(surface_idx) {
    return(list(brain_structure = cifti.structure.canonical(names(surfaces)[surface_idx]),
                surface_number_of_vertices = as.integer(surfaces[surface_idx])))
  }))
}
