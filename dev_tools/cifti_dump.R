# R counterpart of dev_tools/cifti_dump.py: print the same fields, in the same
# order, from the fs.cifti reader, for a text diff.
suppressMessages(devtools::load_all("/home/ts/develop/freesurferformats", quiet = TRUE))

fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("NA")
  return(sprintf("%.6f", as.numeric(x)))
}
fmt_xml <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  # Same normalization as the Python tool: newlines become spaces, and the ends are
  # trimmed (the XML indentation around a multi-line metadata value is not data).
  return(trimws(gsub("\n", " ", value, fixed = TRUE)))
}
dump_idx <- function(values) {
  if (is.null(values)) return("NONE")
  if (is.matrix(values)) values <- as.vector(t(values))
  return(sprintf("%d vals: %s", length(values), paste(as.integer(values), collapse = " ")))
}
dump_data <- function(filepath, dim_sizes) {
  data <- read.cifti(filepath)$data
  if (length(dim(data)) != 2L) {
    return(sprintf("  data SKIPPED (only 2-dimensional matrices are dumped, this one has %d dimensions)", length(dim(data))))
  }
  lines <- sprintf("  data r_type=%s rows=%d cols=%d", typeof(data), nrow(data), ncol(data))
  for (row_idx in seq_len(nrow(data))) {
    lines <- c(lines, sprintf("  data row %d = %s", row_idx - 1L, paste(sprintf("%.6f", data[row_idx, ]), collapse = " ")))
  }
  return(lines)
}
dump_file <- function(filepath) {
  cii <- read.cifti.header(filepath)
  cat(sprintf("file %s\n", filepath))
  cat(sprintf("matrix dim_sizes=%s\n", paste(cii$matrix$dim_sizes, collapse = ",")))
  for (name in names(cii$matrix$metadata)) {
    cat(sprintf("  metadata[%s]=%s\n", name, fmt_xml(cii$matrix$metadata[[name]])))
  }
  for (map_idx in seq_along(cii$matrix$indices_maps)) {
    map <- cii$matrix$indices_maps[[map_idx]]
    cat(sprintf("map %d dims=%s type=%s\n", map_idx - 1L, paste(map$dims, collapse = ","), map$type))
    if (!is.null(map$series)) {
      cat(sprintf("  series points=%d start=%s step=%s exponent=%s unit=%s\n",
                  map$series$number_of_series_points, fmt_num(map$series$start), fmt_num(map$series$step),
                  as.character(map$series$exponent), map$series$unit))
    }
    for (surface in map$surfaces) {
      cat(sprintf("  surface structure=%s num_vertices=%d\n", surface$brain_structure, surface$surface_number_of_vertices))
    }
    for (volume in map$volumes) {
      cat(sprintf("  volume dims=%s meter_exponent=%s\n", paste(volume$dimensions, collapse = ","), as.character(volume$meter_exponent)))
      cat(sprintf("  volume matrix=%s\n", paste(sprintf("%.6f", as.vector(t(volume$transformation_matrix))), collapse = " ")))
    }
    for (bm_idx in seq_along(map$brain_models)) {
      bm <- map$brain_models[[bm_idx]]
      cat(sprintf("  brainmodel %d offset=%d count=%d type=%s structure=%s num_vertices=%s\n",
                  bm_idx - 1L, bm$index_offset, bm$index_count, bm$model_type, bm$brain_structure,
                  as.character(bm$surface_number_of_vertices)))
      cat(sprintf("    vertices %s\n", dump_idx(bm$vertex_indices)))
      cat(sprintf("    voxels %s\n", dump_idx(bm$voxel_indices_ijk)))
    }
    for (parcel_idx in seq_along(map$parcels)) {
      parcel <- map$parcels[[parcel_idx]]
      cat(sprintf("  parcel %d name=%s\n", parcel_idx - 1L, parcel$name))
      for (structure_name in names(parcel$vertices)) {
        cat(sprintf("    vertices structure=%s %s\n", structure_name, dump_idx(parcel$vertices[[structure_name]])))
      }
      cat(sprintf("    voxels %s\n", dump_idx(parcel$voxel_indices_ijk)))
    }
    for (named_map_idx in seq_along(map$named_maps)) {
      named_map <- map$named_maps[[named_map_idx]]
      cat(sprintf("  namedmap %d name=%s\n", named_map_idx - 1L, named_map$name))
      if (!is.null(named_map$labels)) {
        for (label_idx in seq_len(nrow(named_map$labels))) {
          label <- named_map$labels[label_idx, ]
          cat(sprintf("      label key=%d rgba=%s/%s/%s alpha=%s name=%s x=%s y=%s z=%s\n",
                      label$key, fmt_num(label$red), fmt_num(label$green), fmt_num(label$blue),
                      fmt_num(label$alpha), label$label, fmt_num(label$x), fmt_num(label$y), fmt_num(label$z)))
        }
      }
    }
  }
  cat(paste(c(dump_data(filepath, cii$matrix$dim_sizes), ""), collapse = "\n"))
  invisible(NULL)
}
for (filepath in commandArgs(trailingOnly = TRUE)) { dump_file(filepath) }
