# Functions for building CIFTI-2 parcel axes from a parcellation of this package.
#
# A parcellated CIFTI-2 file (`.pscalar`, `.ptseries`, `.pconn`, and the parcellated
# dimension of the mixed types `.pdconn`/`.dpconn`) stores no label table: it describes a
# parcel by its name and by the vertices (per brain structure) and volume voxels it
# contains, and the parcel index is the position in the list of parcels (see
# R/write_cifti_axes.R for the axis builders and R/read_cifti_header.R for what the
# reader makes of this). To write such a file one needs the parcellation in a form that
# assigns a region to every vertex of a surface, which is what the annotations of this
# package are (`fs.annot` instances, see read.fs.annot()). This file turns them into a
# parcels axis; R/write_cifti_connectome.R holds the writers that take such an axis.


#' @title Create a CIFTI-2 parcels axis from brain surface annotations.
#'
#' @description A parcellated CIFTI-2 file stores no label table, it describes each parcel
#'   by its name and by the vertices (per brain structure) and volume voxels it contains.
#'   This function builds such a parcels axis from brain surface annotations (see
#'   \code{\link{read.fs.annot}}), which assign a region label to every vertex of a
#'   hemisphere, so that data that was computed per region of an atlas (a FreeSurfer
#'   parcellation, a Schaefer atlas, ...) can be written to a parcellated file.
#'
#'   The vertices of a parcel are collected by the *name* of the region, not by its label
#'   key: the hemispheres of an atlas usually use the same names for the same region, but
#'   the names often carry a hemisphere marker (e.g. `L_superiorfrontal` in one hemisphere
#'   and `R_superiorfrontal` in the other, or `7Networks_LH_Vis_1` and
#'   `7Networks_RH_Vis_1`), and a parcel of a parcellated CIFTI-2 file is a region that
#'   spans the structures it occurs in. The markers are removed before the names are
#'   compared, see \code{\link{cifti.region.name.without.hemisphere}} for the exact rules.
#'   Vertices whose label is not in the label table of the annotation (the medial wall,
#'   which is usually the 'unknown' region with key 0) form a parcel like any other, named
#'   `default_label_name`: a parcellated file has no 'no value' case, and dropping the
#'   medial wall silently would change the data.
#'
#' @param annots an `fs.annot` instance (see \code{\link{read.fs.annot}}), or a named list
#'   of them, with the brain structures as the names (e.g.
#'   `list(lh = lh_annot, rh = rh_annot)`).
#'
#' @param structure character string or `NULL`, the brain structure of the annotation,
#'   needed if `annots` is a single `fs.annot` instance instead of a named list.
#'
#' @param parcel_names character vector or `NULL`, the names of the parcels to write, in
#'   that order. This selects and orders the parcels of the axis; without it all regions of
#'   the parcellation are used, in the order in which the label table of the annotation
#'   lists them (the atlas order, not the order in which the vertices happen to be stored
#'   in the mesh). Renaming is not supported, since the name is what the hemispheres of a
#'   region are matched by.
#'
#' @param default_label_name character string, the name to use for vertices whose label is
#'   not in the label table of the annotation.
#'
#' @return an axis of type 'CIFTI_INDEX_TYPE_PARCELS', see
#'   \code{\link{cifti.axis.parcels}}, to be passed to \code{\link{write.cifti}} or to one
#'   of the writers that accept it (e.g. \code{\link{write.fs.parcellated.cifti}}).
#'
#' @examples
#' lh_annot_file <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats")
#' lh_annot <- read.fs.annot(lh_annot_file)
#' axis <- cifti.axis.parcels.from.annot(lh_annot, structure = "lh")
#' length(axis$parcels)
#' axis$parcels[[1L]]$name
#'
#' @family cifti functions
#' @export
cifti.axis.parcels.from.annot <- function(annots, structure = NULL, parcel_names = NULL,
                                          default_label_name = "unknown") {
  if (!is.character(default_label_name) || length(default_label_name) != 1L || !nzchar(default_label_name)) {
    stop("Parameter 'default_label_name' must be a single non-empty character string.")
  }
  annot_list <- cifti.annot.list(annots, structure = structure)

  surfaces <- vapply(annot_list, function(annot) {
    return(as.integer(max(annot$vertices) + 1L))
  }, integer(1L))
  names(surfaces) <- names(annot_list)

  vertices_per_parcel <- lapply(names(annot_list), function(structure_name) {
    return(cifti.annot.parcels(annot_list[[structure_name]], structure_name,
                               default_label_name = default_label_name))
  })
  names(vertices_per_parcel) <- names(annot_list)

  # The order of the parcels is the atlas order of the names, and the first structure that
  # contains a name defines its position. This matters, because a region that only occurs
  # in one hemisphere must not move the other regions around.
  parcel_names_available <- unique(unlist(lapply(vertices_per_parcel, names), use.names = FALSE))
  if (!is.null(parcel_names)) {
    if (!is.character(parcel_names) || any(is.na(parcel_names)) || any(!nzchar(parcel_names))) {
      stop("Parameter 'parcel_names' must be a character vector of non-empty parcel names, or NULL.")
    }
    unavailable <- setdiff(parcel_names, parcel_names_available)
    if (length(unavailable) > 0L) {
      stop(sprintf("The parcellation contains no parcel named %s. The annotation%s contain: %s.\n",
                   paste(sQuote(unavailable), collapse = ", "),
                   if (length(annot_list) > 1L) "s" else "",
                   paste(utils::head(parcel_names_available, 20L), collapse = ", ")))
    }
    parcel_names_available <- as.character(parcel_names)
  }

  parcels <- lapply(parcel_names_available, function(parcel_name) {
    parcel_vertices <- list()
    for (structure_name in names(vertices_per_parcel)) {
      vertices <- vertices_per_parcel[[structure_name]][[parcel_name]]
      if (!is.null(vertices) && length(vertices) > 0L) {
        parcel_vertices[[structure_name]] <- as.integer(vertices)
      }
    }
    return(cifti.parcel(parcel_name, vertices = parcel_vertices))
  })
  return(cifti.axis.parcels(parcels, surfaces = surfaces))
}


#' @title Remove hemisphere markers from region names of a parcellation.
#'
#' @description Region names of brain atlases often carry a marker that identifies the
#'   hemisphere a region belongs to, e.g. `L_superiorfrontal` / `R_superiorfrontal`,
#'   `pericalcarine_LH` / `pericalcarine_RH`, or `7Networks_LH_Vis_1` /
#'   `7Networks_RH_Vis_1`. The same region of the two hemispheres has to be recognized as
#'   one parcel when building a parcel axis for a CIFTI-2 file, so these markers are
#'   removed here.
#'
#'   A marker that is removed is one of `L`, `R`, `LH`, `RH`, `Left` or `Right` (matched
#'   case-insensitively), at the start of the name or at its end, followed or preceded by
#'   `_` or `-`, or enclosed by two separators anywhere in the name (in which case the two
#'   separators collapse into one). Names without such a marker are returned unchanged, and
#'   so are names that consist of nothing but a marker.
#'
#' @param region_names character vector, the region names.
#'
#' @return character vector, the names without hemisphere markers.
#'
#' @keywords internal
cifti.region.name.without.hemisphere <- function(region_names) {
  if (!is.character(region_names)) {
    stop("Parameter 'region_names' must be a character vector.")
  }
  markers <- "(LH|RH|LEFT|RIGHT|L|R)"
  separators <- "[_[:space:]-]"
  result <- sub(paste0("^", markers, separators), "", region_names, ignore.case = TRUE)
  result <- sub(paste0(separators, markers, "$"), "", result, ignore.case = TRUE)
  result <- gsub(paste0(separators, markers, separators), "_", result, ignore.case = TRUE)
  return(result)
}


#' @title Collect the vertices of each region of an annotation.
#'
#' @param annot an `fs.annot` instance, see \code{\link{read.fs.annot}}.
#'
#' @param structure_name character string, the brain structure of the annotation, used in
#'   error messages.
#'
#' @param default_label_name character string, the name for vertices without a region name.
#'
#' @return a named list of integer vectors, one entry per region that contains at least one
#'   vertex: the 0-based vertex indices of the region, in ascending order. The entries are
#'   ordered like the label table of the annotation (the atlas order).
#'
#' @keywords internal
cifti.annot.parcels <- function(annot, structure_name, default_label_name = "unknown") {
  if (is.null(annot$label_names)) {
    stop(sprintf(paste0("The annotation for brain structure '%s' has no label names, so its regions cannot be named. A parcellated ",
                        "CIFTI-2 file stores no label table, the parcel name is the only label it has, so the annotation has to be ",
                        "read from a file that contains a colortable (see read.fs.annot()).\n"),
                 cifti.structure.short(structure_name)))
  }
  region_names <- cifti.region.name.without.hemisphere(as.character(annot$label_names))
  missing_name <- is.na(region_names) | !nzchar(region_names)
  region_names[missing_name] <- default_label_name
  vertex_indices <- as.integer(annot$vertices)

  atlas_names <- character(0L)
  if (!is.null(annot$colortable)) {
    atlas_names <- unique(cifti.region.name.without.hemisphere(as.character(annot$colortable$struct_names)))
  }
  used_names <- unique(region_names)
  ordered_names <- c(atlas_names[atlas_names %in% used_names], used_names[!(used_names %in% atlas_names)])

  parcels <- lapply(ordered_names, function(region_name) {
    return(sort(vertex_indices[region_names == region_name]))
  })
  names(parcels) <- ordered_names
  return(parcels)
}


#' @title Accept the input forms of a set of annotations.
#'
#' @param annots an `fs.annot` instance or a named list of them.
#'
#' @param structure character string or `NULL`, the brain structure of a single
#'   annotation.
#'
#' @return a named list of `fs.annot` instances, named by canonical brain structure name.
#'
#' @keywords internal
cifti.annot.list <- function(annots, structure = NULL) {
  if (is.fs.annot(annots)) {
    if (is.null(structure) || !is.character(structure) || length(structure) != 1L || is.na(structure)) {
      stop(paste0("Parameter 'annots' is a single annotation, so the brain structure it belongs to has to be given as a single ",
                  "character string in the parameter 'structure' (e.g. 'lh'). Alternatively pass a named list of annotations, ",
                  "e.g. list(lh = lh_annot, rh = rh_annot).\n"))
    }
    annot_list <- list(annots)
    names(annot_list) <- cifti.structure.canonical(structure)
    return(annot_list)
  }
  if (!is.list(annots) || length(annots) == 0L) {
    stop("Parameter 'annots' must be an fs.annot instance (see read.fs.annot()) or a named list of them.")
  }
  if (is.null(names(annots)) || any(is.na(names(annots))) || any(!nzchar(names(annots)))) {
    stop(paste0("Parameter 'annots' must be a named list of annotations, with the brain structures as the names, e.g. ",
                "list(lh = lh_annot, rh = rh_annot). The structures are required, because the vertex indices of an annotation ",
                "are meaningless without them.\n"))
  }
  annot_list <- lapply(seq_along(annots), function(annot_idx) {
    if (!is.fs.annot(annots[[annot_idx]])) {
      stop(sprintf("The entry '%s' of parameter 'annots' is not an annotation (see read.fs.annot()).\n",
                   names(annots)[annot_idx]))
    }
    return(annots[[annot_idx]])
  })
  names(annot_list) <- vapply(names(annots), cifti.structure.canonical, character(1L))
  if (anyDuplicated(names(annot_list))) {
    stop(sprintf("Parameter 'annots' contains two annotations for the same brain structure (%s).\n",
                 paste(names(annot_list)[duplicated(names(annot_list))], collapse = ", ")))
  }
  return(annot_list)
}
