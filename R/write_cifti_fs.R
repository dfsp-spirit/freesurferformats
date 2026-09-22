# The user-facing CIFTI-2 writers: they take data in the form the package uses for
# morphometry data, time series and parcellations, and build the axes for it. See
# R/write_cifti.R for the generic writer and R/write_cifti_axes.R for the axis builders.


#' @title Write morphometry data to a CIFTI-2 `.dscalar` file.
#'
#' @description Write a per-vertex data vector (or several of them, as the maps of the
#'   file) to a CIFTI-2 dense scalar file. This is the inverse of
#'   \code{\link{read.fs.morph.cifti}}: the data are given for the *complete* surface, and
#'   the vertices that the file contains are selected with the mapping of a template file
#'   (which is what a grayordinates file needs, since it leaves out the medial wall).
#'
#' @param filepath character string, the path of the file to write. The name should end
#'   with `.dscalar.nii`.
#'
#' @param data numeric vector or matrix or named list, the per-vertex data:
#'   \itemize{
#'     \item a vector: one value per vertex of the complete surface of one structure,
#'     \item a matrix: one row per vertex of the complete surface, one column per map,
#'     \item a named list (e.g. `list(lh = ..., rh = ...)`): a vector or matrix as above
#'       per structure.
#'   }
#'   The vertex order is the order of the surface mesh, which is also the order in which
#'   \code{\link{read.fs.morph.cifti}} returns the data.
#'
#' @param template character string (the path of a CIFTI-2 file), an `fs.cifti` or an
#'   `fs.cifti.data` object, or `NULL`. The grayordinate mapping of this file is used, so
#'   pass the file the data comes from (e.g. an HCP file of the same subject): the mapping
#'   of such a file cannot be invented. Without a template, the file covers *all* vertices
#'   of the surfaces, and `structure` has to be given.
#'
#' @param structure character string or `NULL`, the brain structure the data belongs to
#'   (e.g. `'lh'`), needed if `data` is not a named list and there is no template to take
#'   the structures from.
#'
#' @param map_names character vector or `NULL`, the names of the maps. The number of names
#'   has to match the number of maps in the data. Without this, the map names of the
#'   template file are kept if it has as many maps as the data.
#'
#' @param metadata named character vector or `NULL`, the matrix metadata, see
#'   \code{\link{write.cifti}}. The default keeps the metadata of the template file.
#'
#' @return the axes that were written, invisibly.
#'
#' @examples
#' template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' out_file <- file.path(tempdir(), "written.dscalar.nii")
#' data <- read.fs.morph.cifti(template, "lh")
#' data[2] <- 555 # change one value
#' write.fs.morph.cifti(out_file, data, template = template, structure = "lh")
#' read.fs.morph.cifti(out_file, "lh")[1:3]
#' \dontshow{unlink(out_file)}
#'
#' @family cifti functions
#' @export
write.fs.morph.cifti <- function(filepath, data, template = NULL, structure = NULL, map_names = NULL, metadata = NULL) {
  prepared <- cifti.prepare.surface.data(data, structure = structure, template = template, what = "morphometry data")
  axes <- prepared$axes
  axes[[1L]] <- cifti.axis.for.maps(map_names, nx = nrow(prepared$matrix), template_map_axis = prepared$template_map_axis)
  return(write.cifti(filepath, prepared$matrix, axes = axes, template = prepared$template_cii, metadata = metadata))
}


#' @title Write time series data to a CIFTI-2 `.dtseries` file.
#'
#' @description Write a per-vertex time series (or several structures at once) to a CIFTI-2
#'   dense series file. This is the inverse of \code{\link{read.fs.series.cifti}}.
#'
#' @inheritParams write.fs.morph.cifti
#'
#' @param data numeric matrix or named list, the data: a matrix with one row per vertex of
#'   the complete surface and one column per series point (time point), or a named list of
#'   such matrices per structure.
#'
#' @param start numeric, the value of the first series point, see
#'   \code{\link{cifti.axis.series}}.
#'
#' @param step numeric, the difference between consecutive series points.
#'
#' @param unit character string, the unit of the series, one of 'SECOND', 'HERTZ', 'METER'
#'   or 'RADIAN'.
#'
#' @return the axes that were written, invisibly.
#'
#' @examples
#' template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
#' series <- matrix(seq_len(10 * 3), nrow = 10L) # 10 vertices (lh), 3 time points
#' out_file <- file.path(tempdir(), "written.dtseries.nii")
#' write.fs.series.cifti(out_file, series, template = template, structure = "lh", step = 0.72)
#' dim(read.fs.series.cifti(out_file, "lh"))
#' \dontshow{unlink(out_file)}
#'
#' @family cifti functions
#' @export
write.fs.series.cifti <- function(filepath, data, template = NULL, structure = NULL, start = 0, step = 1,
                                  unit = "SECOND", metadata = NULL) {
  prepared <- cifti.prepare.surface.data(data, structure = structure, template = template, what = "time series data")
  axes <- prepared$axes
  axes[[1L]] <- cifti.axis.series(nrow(prepared$matrix), start = start, step = step, unit = unit)
  return(write.cifti(filepath, prepared$matrix, axes = axes, template = prepared$template_cii, metadata = metadata))
}


#' @title Write a parcellation to a CIFTI-2 `.dlabel` file.
#'
#' @description Write per-vertex label keys (one per vertex of the complete surface, per
#'   structure) to a CIFTI-2 dense label file. This is the inverse of
#'   \code{\link{read.fs.parcellation.cifti}}.
#'
#' @inheritParams write.fs.morph.cifti
#'
#' @param data integer vector, matrix or named list, the label keys per vertex, see
#'   `data` in \code{\link{write.fs.morph.cifti}}. The values have to be the label keys of
#'   the label table (they are written as integers, and a key of 0 usually means unknown).
#'
#' @param label_table data.frame or `NULL`, the label table, see
#'   \code{\link{cifti.axis.labels}}: the columns 'key', 'red', 'green', 'blue', 'alpha'
#'   (in the range 0 to 1) and 'label'. The colortable of an `fs.annot` instance (see
#'   \code{\link{read.fs.annot}}) is accepted as well, its colors are then divided by 255.
#'   Without a label table the file still stores the keys, but nothing in it explains what
#'   they mean (Connectome Workbench writes one, and its label files rely on it).
#'
#' @param map_name character string or `NULL`, the name of the label map.
#'
#' @return the axes that were written, invisibly.
#'
#' @examples
#' template <- system.file("extdata", "cifti", "tiny.dlabel.nii", package = "freesurferformats")
#' keys <- read.fs.parcellation.cifti(template, "lh")
#' label_table <- read.fs.parcellation.cifti(template, "lh", with_label_table = TRUE)$label_table
#' out_file <- file.path(tempdir(), "written.dlabel.nii")
#' write.fs.parcellation.cifti(out_file, keys, template = template, structure = "lh",
#'                             label_table = label_table)
#' table(read.fs.parcellation.cifti(out_file, "lh"))
#' \dontshow{unlink(out_file)}
#'
#' @family cifti functions
#' @export
write.fs.parcellation.cifti <- function(filepath, data, template = NULL, structure = NULL, label_table = NULL,
                                        map_name = NULL, metadata = NULL) {
  prepared <- cifti.prepare.surface.data(data, structure = structure, template = template, what = "label keys")
  axes <- prepared$axes
  if (is.null(label_table) && !is.null(prepared$template_map_axis)) {
    # The template knows the label table; keeping it is what makes the keys meaningful.
    label_table <- prepared$template_map_axis$named_maps[[1L]]$labels
    if (is.null(label_table)) {
      label_table <- NULL # nocov
    }
  }
  if (is.null(map_name) && !is.null(prepared$template_map_axis)) {
    map_name <- prepared$template_map_axis$named_maps[[1L]]$name
  }
  if (is.null(map_name) || !nzchar(map_name)) {
    map_name <- ""
  }
  axes[[1L]] <- cifti.axis.labels(map_name, label_tables = list(cifti.label.table.for.writing(label_table)),
                                  metadata = NULL)
  return(write.cifti(filepath, prepared$matrix, axes = axes, template = prepared$template_cii, metadata = metadata))
}


# --- Internal helpers --------------------------------------------------------

#' @title Prepare per-vertex data for writing to a CIFTI-2 file.
#'
#' @description Common work of the user-facing CIFTI-2 writers: turn data that are given
#'   for the complete surface into the matrix of a dense CIFTI-2 file, i.e. one row per map
#'   (or series point, or label map) and one column per brainordinate, in the order of the
#'   grayordinates of the file.
#'
#' @param data numeric vector or matrix, or a named list of them, the per-vertex data.
#'
#' @param structure character string or `NULL`, the structure name for the non-list case.
#'
#' @param template character string, `fs.cifti`, `fs.cifti.data` or `NULL`, the template.
#'
#' @param what character string, a description of the data, used in error messages.
#'
#' @return a named list with the entries 'matrix' (the data matrix to write), 'axes' (the
#'   axes, with the brainordinate axis filled in and the other axis left `NULL`), 'template'
#'   (the template object or `NULL`) and 'template_map_axis' (the axis of the template for
#'   the other matrix dimension, or `NULL`).
#'
#' @keywords internal
cifti.prepare.surface.data <- function(data, structure = NULL, template = NULL, what = "data") {
  template_cii <- NULL
  if (!is.null(template)) {
    template_cii <- cifti.header.of(template)
  }

  structures <- cifti.data.structures(data, structure)
  if (is.null(structures)) {
    stop(sprintf(paste0("The %s are not a named list, so the brain structure they belong to is unknown. Pass the parameter ",
                        "'structure' (e.g. 'lh'), or a named list of per-structure data (e.g. list(lh = ..., rh = ...)).\n"),
                 what))
  }

  template_axes <- NULL
  template_map_axis <- NULL
  if (!is.null(template_cii)) {
    template_axes <- cifti.axis.from.template(template_cii)
    if (length(template_axes) != 2L) {
      stop(sprintf("The template file '%s' has %d matrix dimensions, this writer supports the dense file types with 2 dimensions.\n",
                   template_cii$filepath, length(template_axes))) # nocov
    }
    if (!identical(template_axes[[2L]]$type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")) {
      stop(sprintf(paste0("The second matrix dimension of the template file '%s' is of type '%s', not 'CIFTI_INDEX_TYPE_BRAIN_MODELS', ",
                          "so it does not describe surface vertices. Use a dense file as the template.\n"),
                   template_cii$filepath, template_axes[[2L]]$type))
    }
    template_map_axis <- template_axes[[1L]]
    axis_brain_template <- template_axes[[2L]]
    index_table <- cifti.grayordinates(template_cii, 1L)

    template_structures <- unique(index_table$structure)
    unknown <- setdiff(structures, template_structures)
    if (length(unknown) > 0L) {
      stop(sprintf("The template file '%s' contains no brainordinates for the brain structure(s) %s. It has: %s.\n",
                   template_cii$filepath, paste(cifti.structure.short(unknown), collapse = ", "),
                   paste(cifti.structure.short(template_structures), collapse = ", ")))
    }
    # Only the structures the data are given for end up in the file. Writing the
    # grayordinates of the other structures of the template would require values for them,
    # and there is no missing value in the format (a NaN would be a value).
    kept_structures <- template_structures[template_structures %in% structures]
    kept_models <- axis_brain_template$brain_models[vapply(axis_brain_template$brain_models, function(model) {
      return(model$brain_structure %in% kept_structures)
    }, logical(1L))]
    if (any(vapply(kept_models, function(model) {
      return(!identical(model$model_type, "CIFTI_MODEL_TYPE_SURFACE"))
    }, logical(1L)))) {
      volume_structures <- vapply(kept_models[vapply(kept_models, function(model) {
        return(!identical(model$model_type, "CIFTI_MODEL_TYPE_SURFACE"))
      }, logical(1L))], function(model) cifti.structure.short(model$brain_structure), character(1L))
      stop(sprintf(paste0("The template file '%s' has volume voxels for the brain structure(s) %s, but per-vertex data can only be ",
                          "written for surface brain models (the voxels of a structure are not a set of surface vertices). Use ",
                          "write.cifti() to write data for volume structures.\n"),
                   template_cii$filepath, paste(unique(volume_structures), collapse = ", ")))
    }
    axis_brain <- cifti.axis.brain.models(kept_models, surfaces = cifti.axis.surface.sizes(axis_brain_template))
    index_table <- index_table[index_table$structure %in% kept_structures, , drop = FALSE]
    matrix_rows <- cifti.data.for.grayordinates(data, kept_structures, index_table, what)
  } else {
    matrix_rows <- cifti.data.for.all.vertices(data, structures)
    axis_brain <- cifti.axis.brain.models(lapply(structures, function(structure_name) {
      return(cifti.brain.model.surface(structure_name, cifti.data.surface.size(data, structure_name)))
    }))
  }

  if (any(is.na(matrix_rows))) {
    warning(sprintf(paste0("The %s contain NA values, which are written as NaN: a CIFTI-2 file stores plain floating point ",
                           "numbers and has no missing value.\n"), what))
  }
  return(list(matrix = t(matrix_rows), axes = list(NULL, axis_brain), template_cii = template_cii,
              template_map_axis = template_map_axis))
}


#' @title Get the surface sizes of an axis as a named vector.
#'
#' @param axis an axis of type 'CIFTI_INDEX_TYPE_BRAIN_MODELS'.
#'
#' @return named integer vector or `NULL`, the number of vertices per structure, see
#'   \code{\link{cifti.axis.brain.models}}.
#'
#' @keywords internal
cifti.axis.surface.sizes <- function(axis) {
  if (is.null(axis$surfaces)) {
    return(NULL)
  }
  sizes <- vapply(axis$surfaces, function(surface) {
    return(as.integer(surface$surface_number_of_vertices))
  }, integer(1L))
  names(sizes) <- vapply(axis$surfaces, function(surface) {
    return(cifti.structure.short(surface$brain_structure))
  }, character(1L))
  return(sizes)
}


#' @title Determine the structures of per-vertex data.
#'
#' @param data the data, see \code{\link{write.fs.morph.cifti}}.
#'
#' @param structure character string or `NULL`, the structure for the non-list case.
#'
#' @return character vector of canonical structure names, or `NULL` if the data are not a
#'   named list and no structure was given.
#'
#' @keywords internal
cifti.data.structures <- function(data, structure = NULL) {
  if (is.list(data) && !is.null(names(data)) && !is.data.frame(data)) {
    return(vapply(names(data), function(structure_name) {
      return(cifti.structure.canonical(structure_name))
    }, character(1L)))
  }
  if (is.null(structure)) {
    return(NULL)
  }
  return(cifti.structure.canonical(structure))
}


#' @title Get the per-structure data of one structure.
#'
#' @param data the data, see \code{\link{write.fs.morph.cifti}}.
#'
#' @param structure_name character string, the canonical structure name.
#'
#' @param what character string, a description of the data, used in error messages.
#'
#' @return a matrix with one row per vertex of the complete surface and one column per map.
#'
#' @keywords internal
cifti.data.per.structure <- function(data, structure_name, what) {
  if (is.list(data) && !is.null(names(data)) && !is.data.frame(data)) {
    idx <- which(vapply(names(data), function(structure_name_i) {
      return(identical(cifti.structure.canonical(structure_name_i), structure_name))
    }, logical(1L)))
    if (length(idx) == 0L) {
      stop(sprintf("The %s contain no data for the brain structure '%s'. Available: %s.\n",
                   what, cifti.structure.short(structure_name), paste(names(data), collapse = ", ")))
    }
    entry <- data[[idx[1L]]]
  } else {
    entry <- data
  }
  if (is.null(dim(entry))) {
    if (!is.numeric(entry)) {
      stop(sprintf("The %s for structure '%s' must be numeric.", what, cifti.structure.short(structure_name)))
    }
    return(matrix(as.numeric(entry), ncol = 1L))
  }
  if (length(dim(entry)) != 2L || !is.numeric(entry)) {
    stop(sprintf("The %s for structure '%s' must be a numeric vector or a 2-dimensional numeric matrix (vertices x maps).",
                 what, cifti.structure.short(structure_name)))
  }
  return(matrix(as.numeric(entry), nrow = nrow(entry), ncol = ncol(entry)))
}


#' @title The number of vertices of the surface of one structure.
#'
#' @param data the data, see \code{\link{write.fs.morph.cifti}}.
#'
#' @param structure_name character string, the canonical structure name.
#'
#' @return integer, the number of vertices.
#'
#' @keywords internal
cifti.data.surface.size <- function(data, structure_name) {
  return(as.integer(nrow(cifti.data.per.structure(data, structure_name, what = "data"))))
}


#' @title Select the data values of the grayordinates of a file.
#'
#' @param data the data, see \code{\link{write.fs.morph.cifti}}.
#'
#' @param structures character vector, the canonical structure names, in file order.
#'
#' @param index_table the brainordinate table, see \code{\link{cifti.grayordinates}}.
#'
#' @param what character string, a description of the data, used in error messages.
#'
#' @return a matrix with one row per grayordinate (in file order) and one column per map.
#'
#' @keywords internal
cifti.data.for.grayordinates <- function(data, structures, index_table, what) {
  blocks <- list()
  for (structure_name in structures) {
    structure_data <- cifti.data.per.structure(data, structure_name, what)
    rows <- which(index_table$structure == structure_name)
    if (length(rows) == 0L) {
      next
    }
    if (any(index_table$model_type[rows] != "SURFACE")) {
      stop(sprintf(paste0("The template file contains volume voxels for the brain structure '%s', but per-vertex data can only be ",
                          "written for surface brain models. Use write.cifti() to write data for volume structures.\n"),
                   cifti.structure.short(structure_name)))
    }
    vertex_indices <- index_table$vertex_index[rows] + 1L # the vertex indices in the table are 0-based
    if (max(vertex_indices) > nrow(structure_data)) {
      stop(sprintf(paste0("The template file refers to vertex %d of the brain structure '%s', but the data for that structure have ",
                          "only %d vertices. The data have to be given for the complete surface of the template.\n"),
                   max(vertex_indices) - 1L, cifti.structure.short(structure_name), nrow(structure_data)))
    }
    blocks[[length(blocks) + 1L]] <- structure_data[vertex_indices, , drop = FALSE]
  }
  if (length(blocks) == 0L) {
    stop(sprintf("The %s contain none of the brain structures the template file has (%s).\n",
                 what, paste(unique(cifti.structure.short(index_table$structure)), collapse = ", ")))
  }
  return(do.call(rbind, blocks))
}


#' @title Assemble the data values of all vertices for a file without a template.
#'
#' @param data the data, see \code{\link{write.fs.morph.cifti}}.
#'
#' @param structures character vector, the canonical structure names, in file order.
#'
#' @param what character string, a description of the data, used in error messages.
#'
#' @return a matrix with one row per vertex (structures one after the other) and one column
#'   per map.
#'
#' @keywords internal
cifti.data.for.all.vertices <- function(data, structures, what = "data") {
  blocks <- lapply(structures, function(structure_name) {
    return(cifti.data.per.structure(data, structure_name, what))
  })
  return(do.call(rbind, blocks))
}


#' @title Build the brainordinate table of an axis.
#'
#' @param axis an axis of type 'CIFTI_INDEX_TYPE_BRAIN_MODELS'.
#'
#' @return a data.frame, see \code{\link{cifti.grayordinates}}.
#'
#' @keywords internal
cifti.grayordinates.for.axes <- function(axis) {
  tables <- lapply(axis$brain_models, function(model) {
    return(cifti.grayordinates.for.model(list(volumes = axis$volumes), model))
  })
  result <- do.call(rbind, tables)
  rownames(result) <- NULL
  return(result)
}


#' @title Build the axis for a set of maps.
#'
#' @param map_names character vector or `NULL`, the map names.
#'
#' @param nx integer, the number of maps in the data.
#'
#' @param template_map_axis the axis of the template file for this dimension, or `NULL`.
#'   If it has as many maps as the data, its map names and the per-map metadata (e.g. the
#'   palette information) are kept.
#'
#' @return an axis, see \code{\link{cifti.axis.scalars}}.
#'
#' @keywords internal
cifti.axis.for.maps <- function(map_names, nx, template_map_axis = NULL) {
  if (is.null(map_names)) {
    if (!is.null(template_map_axis) && length(template_map_axis$named_maps) == nx) {
      names <- vapply(template_map_axis$named_maps, function(named_map) {
        return(as.character(named_map$name))
      }, character(1L))
      metadata <- lapply(template_map_axis$named_maps, function(named_map) {
        return(named_map$metadata)
      })
      return(cifti.axis.scalars(names, metadata = metadata))
    }
    map_names <- rep("", nx)
  }
  if (length(map_names) != nx) {
    stop(sprintf("The data have %d maps, but %d map names were given.\n", nx, length(map_names)))
  }
  return(cifti.axis.scalars(map_names))
}


#' @title Convert a label table to the format the writer expects.
#'
#' @param label_table a data.frame, see \code{\link{cifti.axis.labels}}, the colortable of
#'   an `fs.annot` instance, or `NULL`. The column names are matched case-insensitively, so
#'   the label table that \code{\link{read.fs.parcellation.cifti}} returns (with the columns
#'   'Key', 'Red', 'Green', 'Blue', 'Alpha' and 'Label') can be passed directly. The
#'   colortable of a FreeSurfer annotation stores the colors in the range 0 to 255, and it
#'   is converted here.
#'
#' @return a data.frame with the columns 'key', 'red', 'green', 'blue', 'alpha' and
#'   'label', or `NULL`.
#'
#' @keywords internal
cifti.label.table.for.writing <- function(label_table) {
  if (is.null(label_table)) {
    return(NULL)
  }
  if (is.data.frame(label_table)) {
    names(label_table) <- tolower(names(label_table))
    return(cifti.check.label.table(label_table, map_idx = 1L))
  }
  if (is.list(label_table) && !is.null(label_table$struct_names) && !is.null(label_table$table)) {
    # The colortable of a FreeSurfer annotation, see read.fs.annot(): the table holds
    # r, g, b, a in the range 0 to 255 and the label key in the last column.
    colors <- label_table$table[, 1:4, drop = FALSE]
    if (any(colors > 1., na.rm = TRUE)) {
      colors <- colors / 255.
    }
    return(cifti.check.label.table(data.frame(
      key = as.integer(label_table$table[, ncol(label_table$table)]),
      red = colors[, 1L], green = colors[, 2L], blue = colors[, 3L], alpha = colors[, 4L],
      label = as.character(label_table$struct_names), stringsAsFactors = FALSE), map_idx = 1L))
  }
  stop("Parameter 'label_table' must be a data.frame (see cifti.axis.labels()) or the colortable of an fs.annot instance, or NULL.")
}
