#' @title Write file in FreeSurfer weight format
#'
#' @description Write vertex-wise brain data for a set of vertices to file in *weight* format. This format is also known as *paint* format or simply as *w* format.
#'
#' @param filepath, string. Full path to the output weight file.
#'
#' @param vertex_indices vector of integers, the vertex indices. Must be one-based (R-style). This function will substract 1, as they need to be stored zero-based in the file.
#'
#' @param values vector of floats. The brain morphometry data to write, one value per vertex.
#'
#' @param format character string, one of 'bin' or 'asc'. The weight format type, there is a binary version of the format and an ASCII version.
#'
#' @family morphometry functions
#'
#' @export
write.fs.weight <- function(filepath, vertex_indices, values, format = 'bin') {

  if(!(format %in% c('bin', 'asc'))) {
    stop("Format must be one of c('bin', 'asc').");
  }

  if(format == 'asc') {
    return(invisible(write.fs.weight.asc(filepath, vertex_indices, values)));
  }

  num_verts = length(vertex_indices);

  vertex_indices = as.integer(vertex_indices);
  if(min(vertex_indices) < 1L) {
    stop("Vertex indices must be passes one-based.");
  }
  vertex_indices = vertex_indices - 1L;  # transform to zero-based indices

  if(num_verts != length(values)) {
    stop(sprintf("Received %d vertex indices but %d values, lengths must match.\n", num_verts, length(values)));
  }

  fh = file(filepath, "wb", blocking = TRUE);
  latency = 0L;

  writeBin(as.integer(latency), fh, size = 2, endian = "big");
  fwrite3(fh, num_verts);

  for(write_idx in seq_len(num_verts)) {
    fwrite3(fh, vertex_indices[write_idx]);
    writeBin(as.double(values[write_idx]), fh, size =4, endian = "big");
  }

  close(fh);
}


#' @title Write file in FreeSurfer weight ASCII format
#'
#' @description Write vertex-wise brain data for a set of vertices to an ASCII file in *weight* format. This format is also known as *paint* format or simply as *w* format.
#'
#' @param filepath, string. Full path to the output ASCII weight file.
#'
#' @param vertex_indices vector of integers, the vertex indices. Must be one-based (R-style). This function will substract 1, as they need to be stored zero-based in the file.
#'
#' @param values vector of floats. The brain morphometry data to write, one value per vertex.
#'
#' @family morphometry functions
#'
#' @export
write.fs.weight.asc <- function(filepath, vertex_indices, values) {

  num_verts = length(vertex_indices);

  vertex_indices = as.integer(vertex_indices);

  if(min(vertex_indices) < 1L) {
    stop("Vertex indices must be passes one-based.");
  }
  vertex_indices = vertex_indices - 1L;  # transform to zero-based indices



  if(num_verts != length(values)) {
    stop(sprintf("Received %d vertex indices but %d values, lengths must match.\n", num_verts, length(values)));
  }

  count_line = sprintf("%d", num_verts);

  fh = file(filepath, "w");
  writeLines(c("0", count_line), fh);  # the first line containing only a '0' is the latency line.
  close(fh);

  # Append the vertex data
  weight_df = data.frame('vertex_index'=vertex_indices, 'weight'=values);
  write.table(weight_df, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
}

