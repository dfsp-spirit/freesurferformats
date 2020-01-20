#' @title Write file in FreeSurfer weight format
#'
#' @description Write vertex-wise brain data for a set of vertices to a binary file in *weight* format. This format is also known as *paint* format or simply as *w* format.
#'
#' @param filepath, string. Full path to the output weight file.
#'
#' @param vertex_indices vector of integers, the vertex indices. Must be one-based (R-style). This function will substract 1, as they need to be stored zero-based in the file.
#'
#' @param values vector of floats. The brain morphometry data to write, one value per vertex.
#'
#' @family morphometry functions
#'
#' @export
write.fs.weight <- function(filepath, vertex_indices, values) {

  num_verts = length(vertex_indices);

  vertex_indices = as.integer(vertex_indices);
  vertex_indices = vertex_indices - 1L;  # transform to zero-based indices

  if(min(vertex_indices < 1L)) {
    stop("Vertex indices must be passes one-based.");
  }

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
