#' @title Read file in FreeSurfer label format
#'
#' @description Read a mask in FreeSurfer label format.
#'    A label defines a list of vertices (of an associated surface or morphometry file) which are part of it. All others are not. You can think of it as binary mask. Label files are ASCII text files, which have 5 columns (vertex index, coord1, coord2, coord3, value), but only the vertex indices are of interest.
#'
#' @param filepath string. Full path to the input label file.
#'
#' @param return_one_based_indices logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them.
#'
#' @param full logical, whether to return a full object of class `fs.label` instead of only a vector containing the vertex indices. If TRUE, a named list with the following two entries is returned: 'one_based_indices': logical, whether the vertex indices are one-based. 'vertexdata': a data.frame with the following columns: 'vertex_index': integer, see parameter 'return_one_based_indices', 'coord1', 'coord2', 'coord3': float coordinates, 'value': float, scalar data for the vertex, can mean anything. This parameter defaults to FALSE.
#'
#' @param metadata named list of arbitrary metadata to store in the instance, ignored unless the paramter `full` is TRUE.
#'
#' @return vector of integers or `fs.label` instance (see parameter `full`). The vertex indices from the label file. See the parameter `return_one_based_indices` for important information regarding the start index.
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
read.fs.label <- function(filepath, return_one_based_indices=TRUE, full=FALSE, metadata=list()) {

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
      vertices_df$vertex_index = vertices;
    }
    if(full) {
      ret_list = list("vertexdata"=vertices_df, "metadata"=metadata);
      if(return_one_based_indices) {
        ret_list$one_based_indices = TRUE;
      } else {
        ret_list$one_based_indices = FALSE;
      }

      class(ret_list) = c('fs.label', class(ret_list));
      return(ret_list);
    } else {
      return(vertices);
    }
}


#' @title Print description of a brain surface label.
#'
#' @param x brain surface label with class `fs.label`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.label <- function(x, ...) {
  if(nrow(x$vertexdata) > 0L) {
    vertex_data_range = range(x$vertexdata$value);
    cat(sprintf("Brain surface label containing %d vertices, vertex data values are in range (%.3f, %.3f). Summary:\n", nrow(x$vertexdata), vertex_data_range[1], vertex_data_range[2]));
    print(summary(x$vertexdata$value));
    if(x$one_based_indices) {
      cat(sprintf("Vertex indices start at: 1\n"));
    } else {
      cat(sprintf("Vertex indices start at: 0\n"));
    }
    cat(sprintf("Label vertex coordinates: minimal values are (%.2f, %.2f, %.2f), maximal values are (%.2f, %.2f, %.2f).\n", min(x$vertexdata$coord1), min(x$vertexdata$coord2), min(x$vertexdata$coord3), max(x$vertexdata$coord1), max(x$vertexdata$coord2), max(x$vertexdata$coord3)));
  } else {
    cat(sprintf("Brain surface label containing %d vertices.\n", nrow(x$vertexdata)));
  }
}


#' @title Check whether object is an fs.label
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain surface label (that is, has `fs.label` amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.label <- function(x) inherits(x, "fs.label")


