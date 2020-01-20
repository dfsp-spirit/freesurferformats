#' @title Read file in FreeSurfer weight or w format
#'
#' @description Read morphometry data in weight format (aka `w` files). A weight format file contains morphometry data for a set of vertices, defined by their index in a surface. This can be only a **subset** of the surface vertices.
#'
#' @param filepath string. Full path to the input weight file. Weight files typically have the file extension '.w', but that is not enforced.
#'
#' @param format one of 'auto', 'asc', or 'bin'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.w.asc'.
#'
#' @return the indices and weight data, as a named list. Entries: "vertex_indices": vector of *n* vertex indices. They are stored zero-based in the file, but are returned one-based (R-style). "value": double vector of length *n*, the morphometry data for the vertices. The data can be whatever you want.
#'
#' @family morphometry functions
#'
#' @export
read.fs.weight <- function(filepath, format='auto') {

  if(!(format %in% c('auto', 'bin', 'asc'))) {
    stop("Format must be one of c('auto', 'bin', 'asc').");
  }

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(read.fs.weight.asc(filepath));
  }


  if(filepath.ends.with(filepath, c(".w.asc"))) {
    return(read.fs.weight.asc(filepath));
  }

  if(guess.filename.is.gzipped(filepath)) {
    fh = gzfile(filepath, "rb");
  } else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();

  latency = readBin(fh, integer(), size = 2L, n = 1, endian = "big");
  num_vertex_value_pairs = fread3(fh);

  #message(sprintf("Latency=%d, about to read data for %d vertices.\n", latency, num_vertex_value_pairs));

  vertex_index = rep(0L, num_vertex_value_pairs);
  vertex_value = rep(0.0, num_vertex_value_pairs);

  for(read_idx in seq_len(num_vertex_value_pairs)) {
    vertex_index[read_idx] = fread3(fh);
    vertex_value[read_idx] = readBin(fh, numeric(), size = 4, n = 1, endian = "big");
  }
  return(list("vertex_indices"=vertex_index + 1, "values"=vertex_value));
}


#' Read ASCII version of FreeSurfer weight file.
#'
#' @description Called by \code{\link[freesurferformats]{read.fs.weight}} if parameter `format` is set to 'asc'.
#'
#' @param filepath string. Full path to the input weight file.
#'
#' @return the indices and weight data, as a named list. Entries: "vertex_indices": vector of *n* vertex indices. They are stored zero-based in the file, but are returned one-based (R-style). "value": double vector of length *n*, the morphometry data for the vertices. The data can be whatever you want.
#'
#' @keywords internal
read.fs.weight.asc <- function(filepath) {
  # The first line contains the unused 'latency' value, we do not read it at all.
  num_pairs_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_vertex_value_pairs'), colClasses = c("integer"));
  num_pairs = num_pairs_df$num_vertex_value_pairs[1];

  pairs_df = read.table(filepath, skip=2L, col.names = c('vertex_index', 'value'), colClasses = c("integer", "numeric"), nrows=num_pairs);

  if(nrow(pairs_df) != num_pairs) {
    stop(sprintf("Expected %d vertex values from ASCII weight file header, but received %d.\n", num_pairs, nrow(pairs_df)));
  }
  return(list("vertex_indices"=pairs_df$vertex_index + 1, "values"=pairs_df$value));
}


