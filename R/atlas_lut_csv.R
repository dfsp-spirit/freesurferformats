# Functions for constructing brain atlases (annotations) from a colortable (LUT) file and a per-vertex label file.


#' @title Construct a brain atlas from a colortable (LUT) file and a per-vertex label file.
#'
#' @description Construct a brain atlas (surface annotation) from a FreeSurfer ASCII colortable lookup table (LUT) file and a text file that assigns a label (struct index) to each vertex of a brain surface. This is useful when an atlas is distributed as two text files: a colortable (LUT) plus a per-vertex label assignment, for example the cortical atlases bundled with the Python package 'yabplot' (files like 'aparc_LUT.txt' and 'aparc_conte69.csv').
#'
#' @param lut_file string, path to a colortable file in FreeSurfer ASCII LUT format (see \code{\link[freesurferformats]{read.fs.colortable}}). The file must contain the columns 'struct_index', 'struct_name', 'r', 'g', 'b', 'a', as in the 'FreeSurferColorLUT.txt' files. The struct index is the region identifier used in the label file.
#'
#' @param csv_file string, path to a text file that assigns a label to each vertex. The file must contain one integer per line: the struct index (label ID) of the region for that vertex, as defined in the first column of the LUT file. See parameter \code{unknown_index} for how unlabeled vertices are encoded. If you already have the indices in memory, use parameter \code{label_indices} instead.
#'
#' @param label_indices integer vector, an alternative to parameter 'csv_file'. The per-vertex struct indices, one per vertex. If given, 'csv_file' is ignored.
#'
#' @param num_vertices integer, optional. The expected number of vertices. If given, it must match the number of label indices read from 'csv_file' or 'label_indices'.
#'
#' @param unknown_label_name string, the name to use for unlabeled vertices (e.g., the medial wall). Defaults to 'unknown'.
#'
#' @param unknown_index integer, the struct index that is used in the label file to mark unlabeled vertices. Defaults to 0.
#'
#' @param include_unknown_in_colortable logical, whether to add a region for unlabeled vertices to the colortable of the returned annotation, in case the LUT file does not already contain a region with the 'unknown_index'. Defaults to TRUE. This ensures that vertices without a valid label (e.g., the medial wall) get a proper region name and color in the returned annotation.
#'
#' @return an 'fs.annot' instance with class 'fs.annot', see \code{\link[freesurferformats]{read.fs.annot}} for the structure. Can be written to a FreeSurfer annotation file with \code{\link[freesurferformats]{write.fs.annot}}.
#'
#' @family atlas functions
#'
#' @examples
#' \dontrun{
#' # Example for the yabplot cortical atlas files (LUT + per-vertex indices):
#' annot <- atlas.from.lut.and.csv("aparc_LUT.txt", "aparc_conte69.csv")
#' write.fs.annot("lh.aparc.annot", fs.annot = annot)
#' }
#'
#' @export
atlas.from.lut.and.csv <- function(lut_file, csv_file = NULL, label_indices = NULL, num_vertices = NULL,
                                   unknown_label_name = "unknown", unknown_index = 0L,
                                   include_unknown_in_colortable = TRUE) {

  # --- read the colortable from the LUT file ---
  if (is.null(lut_file) || !file.exists(lut_file)) {
    stop("Parameter 'lut_file' must point to an existing colortable file in FreeSurfer ASCII LUT format.")
  }
  colortable_df <- read.fs.colortable(lut_file, compute_colorcode = TRUE)
  if (is.null(colortable_df$code)) { # safety: compute the color codes if not present
    colortable_df$code <- colortable_df$r + colortable_df$g * 2^8 + colortable_df$b * 2^16 + colortable_df$a * 2^24
  }

  # --- read the per-vertex label indices ---
  if (is.null(label_indices)) {
    if (is.null(csv_file) || !file.exists(csv_file)) {
      stop("Either parameter 'csv_file' (path to an existing file) or 'label_indices' (integer vector) must be given.")
    }
    label_indices <- scan(csv_file, quiet = TRUE)
  }
  if (!is.numeric(label_indices)) {
    stop("Parameter 'label_indices' must be a numeric vector (or come from a valid 'csv_file').")
  }
  label_indices <- as.integer(label_indices)

  if (!is.null(num_vertices)) {
    if (length(label_indices) != num_vertices) {
      stop(sprintf("Number of label indices (%d) does not match parameter 'num_vertices' (%d).\n", length(label_indices), num_vertices))
    }
  }

  # --- make sure there is a region for the unlabeled vertices (e.g., medial wall) ---
  if (include_unknown_in_colortable && !(unknown_index %in% colortable_df$struct_index)) {
    unknown_row <- data.frame("struct_index" = unknown_index, "struct_name" = unknown_label_name,
                              "r" = 0L, "g" = 0L, "b" = 0L, "a" = 0L, "code" = 0L, stringsAsFactors = FALSE)
    colortable_df <- rbind(unknown_row, colortable_df) # prepend, like in standard FreeSurfer annotations
  }

  # --- map each vertex index to its row in the colortable via the struct index ---
  idx2row <- match(label_indices, colortable_df$struct_index)

  # --- build the fs.annot instance ---
  num_vertices_final <- length(label_indices)
  vertices <- seq(0L, num_vertices_final - 1L)

  colortable <- list(
    "num_entries" = nrow(colortable_df),
    "struct_names" = as.character(colortable_df$struct_name),
    "table" = as.matrix(colortable_df[, c("r", "g", "b", "a", "code")]),
    "struct_index" = colortable_df$struct_index
  )

  hex_color_string_rgb <- grDevices::rgb(colortable_df$r / 255, colortable_df$g / 255, colortable_df$b / 255)
  hex_color_string_rgba <- grDevices::rgb(colortable_df$r / 255, colortable_df$g / 255, colortable_df$b / 255, colortable_df$a / 255)
  colortable_df$hex_color_string_rgb <- hex_color_string_rgb
  colortable_df$hex_color_string_rgba <- hex_color_string_rgba
  # reorder columns to match the 'colortable_df' convention of read.fs.annot()
  colortable_df <- colortable_df[, c("struct_name", "r", "g", "b", "a", "code", "hex_color_string_rgb", "hex_color_string_rgba", "struct_index")]

  # Per-vertex label codes, names and hex colors, derived directly from the struct index -> row mapping.
  # (Mapping by color code alone would be ambiguous for atlases in which several regions share a color,
  #  e.g., the left/right hemisphere regions of the Desikan-Killiany 'aparc' atlas.)
  label_codes <- colortable_df$code[idx2row]
  label_codes[is.na(label_codes)] <- 0L # vertices with an index not present in the LUT get the 'unknown' code (0)
  label_names <- as.character(colortable_df$struct_name[idx2row])
  label_names[is.na(label_names)] <- unknown_label_name
  hex_colors_rgb <- hex_color_string_rgb[idx2row]
  hex_colors_rgb[is.na(hex_colors_rgb)] <- "#333333"

  return_list <- list(
    "vertices" = vertices,
    "label_codes" = label_codes,
    "label_names" = label_names,
    "hex_colors_rgb" = hex_colors_rgb,
    "metadata" = list("source_lut_file" = lut_file, "source_csv_file" = if (is.null(csv_file)) "" else csv_file, "label_indices" = label_indices),
    "colortable" = colortable,
    "colortable_df" = colortable_df
  )
  class(return_list) <- c("fs.annot", class(return_list))
  return(return_list)
}


#' @title Write a brain atlas to a colortable (LUT) file and a per-vertex label file.
#'
#' @description Write a brain atlas (surface annotation) to two text files: a FreeSurfer ASCII colortable lookup table (LUT) file and a per-vertex label file. This is the inverse of \code{\link[freesurferformats]{atlas.from.lut.and.csv}}, and is useful for exporting an annotation in the simple text format used e.g. by the Python package 'yabplot' (files like 'aparc_LUT.txt' and 'aparc_conte69.csv'). If the annotation was created by \code{\link[freesurferformats]{atlas.from.lut.and.csv}}, the original per-vertex label indices are stored in its metadata and are used for the export, which makes it lossless. Otherwise, the indices are recovered by matching the per-vertex color codes against the colortable, which is ambiguous if several regions share a color code (e.g., the left/right pairs of the Desikan-Killiany 'aparc' atlas).
#'
#' @param fs.annot an annotation, as returned by \code{\link[freesurferformats]{read.fs.annot}} or \code{\link[freesurferformats]{atlas.from.lut.and.csv}}. Must have a colortable (an entry named 'colortable_df').
#'
#' @param lut_file string, path to the output colortable file. Will be written in FreeSurfer ASCII LUT format, see \code{\link[freesurferformats]{write.fs.colortable}}.
#'
#' @param csv_file string, path to the output per-vertex label file. Will contain one integer per line: the struct index (label ID) of the region for that vertex, as defined in the first column of the LUT file. Unlabeled vertices (e.g., the medial wall) are encoded with the struct index given in parameter \code{unknown_index}.
#'
#' @param unknown_index integer, the struct index to use for unlabeled vertices (i.e., vertices whose label code has no entry in the colortable). Defaults to 0.
#'
#' @return named list with the following entries: 'lut_file' and 'csv_file' (the paths to the files that were written), 'num_vertices' (integer, the number of vertices in the per-vertex label file) and 'num_regions' (integer, the number of regions in the LUT file).
#'
#' @family atlas functions
#'
#' @examples
#' \dontrun{
#' annot <- read.fs.annot("lh.aparc.annot")
#' write.atlas.to.lut.and.csv(annot, "myatlas_LUT.txt", "myatlas_vertices.csv")
#' # read it back into an annotation:
#' annot2 <- atlas.from.lut.and.csv("myatlas_LUT.txt", "myatlas_vertices.csv")
#' }
#'
#' @export
write.atlas.to.lut.and.csv <- function(fs.annot, lut_file, csv_file, unknown_index = 0L) {
  if (!is.fs.annot(fs.annot)) {
    stop("Parameter 'fs.annot' must be an annotation (class 'fs.annot'), as returned by read.fs.annot().")
  }
  if (is.null(fs.annot$colortable_df)) {
    stop("The annotation 'fs.annot' must have a colortable (an entry named 'colortable_df').")
  }

  # --- write the colortable to the LUT file (standard FreeSurfer ASCII LUT column order) ---
  ct <- fs.annot$colortable_df
  lut <- data.frame(
    "struct_index" = ct$struct_index,
    "struct_name" = as.character(ct$struct_name),
    "r" = as.integer(ct$r),
    "g" = as.integer(ct$g),
    "b" = as.integer(ct$b),
    "a" = as.integer(ct$a),
    stringsAsFactors = FALSE
  )
  write.fs.colortable(lut_file, lut)

  # --- write the per-vertex struct indices to the label file ---
  # Prefer the per-vertex label indices stored in the metadata (set by atlas.from.lut.and.csv()):
  # they preserve the exact mapping even when several regions share a color code. Otherwise, fall
  # back to recovering the indices by matching the label codes against the colortable.
  label_indices <- fs.annot$metadata$label_indices
  if (is.null(label_indices) || length(label_indices) != length(fs.annot$label_codes)) {
    if (anyDuplicated(ct$code)) {
      warning("The colortable contains regions that share a color code (e.g., left/right hemisphere pairs). Per-vertex label indices cannot be recovered exactly from the color codes, so the first region with a given code is used for all matching vertices.")
    }
    row_idx <- match(fs.annot$label_codes, ct$code)
    label_indices <- ct$struct_index[row_idx]
    label_indices[is.na(label_indices)] <- unknown_index # map vertices with unknown codes to the unknown index
  }
  writeLines(as.character(label_indices), con = csv_file)

  return(invisible(list("lut_file" = lut_file, "csv_file" = csv_file, "num_vertices" = length(label_indices), "num_regions" = nrow(lut))))
}
