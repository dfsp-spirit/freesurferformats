#' @title Read file in FreeSurfer label format
#'
#' @description Read a mask in FreeSurfer label format.
#'    A label defines a list of vertices (of an associated surface or morphometry file) which are part of it. All others are not. You can think of it as binary mask. Label files are ASCII text files, which have 5 columns (vertex index, coord1, coord2, coord3, value), but only the vertex indices are of interest.
#'
#' @param filepath string. Full path to the input label file.
#'
#' @param return_one_based_indices logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them.
#'
#' @return vector of integers. The vertex indices from the label file. See the parameter 'return_one_based_indices' for important information regarding the start index.
#'
#' @family label functions
#'
#' @examples
#'     labelfile = system.file("extdata", "lh.entorhinal_exvivo.label",
#'       package = "freesurferformats", mustWork = TRUE);
#'     label = read.fs.label(labelfile);
#'
#' @export
#' @importFrom utils read.table
read.fs.label <- function(filepath, return_one_based_indices=TRUE) {

    # The first line is a comment, and the 2nd one contains a single number: the number of vertex lines following.
    num_verts_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts'), colClasses = c("integer"));
    num_verts = num_verts_df$num_verts[1];

    vertices_df = read.table(filepath, skip=2L, col.names = c('vertex_index', 'coord1', 'coord2', 'coord3', 'value'), colClasses = c("integer", "numeric", "numeric", "numeric", "numeric"));
    vertices = vertices_df$vertex_index;

    if(length(vertices) != num_verts) {
      stop(sprintf("Expected %d vertex rows in label file '%s' from header, but received %d.\n", num_verts, filepath, length(vertices)));
    }

    if(return_one_based_indices) {
      vertices = vertices + 1L;
    }
    return(vertices);
}
