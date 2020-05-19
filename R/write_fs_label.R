#' @title Write vertex indices to file in FreeSurfer label format
#'
#' @description Write vertex coordinates and vertex indices defining faces to a file in FreeSurfer binary surface format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/label/lh.cortex'.
#'
#' @param filepath string. Full path to the output label file. If it ends with ".gz", the file is written in gzipped format. Note that this is not common, and that other software may not handle this transparently.
#'
#' @param vertex_indices instance of class `fs.label` or an integer vector, the label. The vertex indices included in the label. As returned by \code{\link[freesurferformats]{read.fs.label}}.
#'
#' @param vertex_coords an *n* x 3 float matrix of vertex coordinates, where *n* is the number of 'vertex_indices'. Optional, defaults to NULL, which will write placeholder data. The vertex coordinates are not used by any software I know (you should get them from the surface file). Will be used from `fs.label` instance if given.
#'
#' @param vertex_data a numerical vector of length *n*, where *n* is the number of 'vertex_indices'. Optional, defaults to NULL, which will write placeholder data. The vertex data are not used by any software I know (you should get them from a morphometry file). Will be used from `fs.label` instance if given.
#'
#' @param indices_are_one_based logical, whether the given indices are one-based, as is standard in R. Indices are stored zero-based in label files, so if this is TRUE, all indices will be incremented by one before writing them to the file. Defaults to TRUE. If FALSE, it is assumed that they are zero-based and they are written to the file as-is.  Will be used from `fs.label` instance if given.
#'
#' @return dataframe, the dataframe that was written to the file (after the header lines).
#'
#' @family label functions
#'
#' @examples
#' \donttest{
#'     # Write a simple label containing only vertex indices:
#'     label_vertices = c(1,2,3,4,5,1000,2000,2323,34,34545,42);
#'     write.fs.label(tempfile(fileext=".label"), label_vertices);
#'
#'     # Load a full label, write it back to a file:
#'     labelfile = system.file("extdata", "lh.entorhinal_exvivo.label",
#'      package = "freesurferformats", mustWork = TRUE);
#'     label = read.fs.label(labelfile, full=TRUE);
#'     write.fs.label(tempfile(fileext=".label"), label);
#' }
#'
#' @importFrom utils write.table
#' @export
write.fs.label <- function(filepath, vertex_indices, vertex_coords=NULL, vertex_data=NULL, indices_are_one_based=TRUE) {

  if(is.fs.label(vertex_indices)) {
    label = vertex_indices;
    vertex_indices = label$vertexdata$vertex_index;
    vertex_coords = as.matrix(data.frame(label$vertexdata$coord1, label$vertexdata$coord2, label$vertexdata$coord3));
    vertex_data = label$vertexdata$value;
    indices_are_one_based = label$one_based_indices;
  }

  min_ind = min(vertex_indices);
  if(min_ind < 0) {
    stop("The vertex_indices to write to the label file must not contain negative values.");
  }
  if(indices_are_one_based && min_ind < 1) {
    stop("The vertex_indices to write to the label file must not contain indices < 1 if the parameter 'indices_are_one_based' is set to TRUE.");
  }


  num_verts = length(vertex_indices);

  if(is.null(vertex_coords)) {
    vertex_coords = matrix(rep(1.0, num_verts * 3), nrow=num_verts);
  }

  if(nrow(vertex_coords) != num_verts) {
    stop(sprintf("Found %d vertex_indices but %d rows of vertex_coords. Numbers must match. Cannot write label file.\n", num_verts, nrow(vertex_coords)));
  }


  if(is.null(vertex_data)) {
    vertex_data = rep(1.0, num_verts);
  }

  if(length(vertex_data) != num_verts) {
    stop(sprintf("Found %d vertex_indices but %d vertex_data values. Numbers must match. Cannot write label file.\n", num_verts, length(vertex_data)));
  }

  if(indices_are_one_based) {
    vertex_indices = vertex_indices - 1;
  }

  label_df = data.frame("vertex_indices"=vertex_indices, "x"=vertex_coords[,1], "y"=vertex_coords[,2], "z"=vertex_coords[,3], "data"=vertex_data);


  # Write the first comment line and the 2nd line containing the number of vertices in the label
  fh =  file(filepath);
  writeLines(c("#!ascii label for subject anonymous", sprintf("%d", length(vertex_indices))), fh);
  close(fh);

  # Append the data
  write.table(label_df, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  return(invisible(label_df));
}


#' @title Write a binary surface label in GIFTI format.
#'
#' @description The data will be written with intent 'NIFTI_INTENT_LABEL' and as datatype 'NIFTI_TYPE_INT32'. The label table will include entries 'positive' (label value 0), and 'negative' (label value 1).
#'
#' @param filepath string, the full path of the output GIFTI file.
#'
#' @param vertex_indices integer vector, the vertex indices which are part of the label (positive). All others not listed, up to num_vertices_in_surface, will be set to be negative.
#'
#' @param num_vertices_in_surface integer, the total number of vertices in the surface mesh. A GIFTI label is more like a mask/an annotation, so we need to know the number of vertices.
#'
#' @return format, string. The format that was used to write the data: "gii".
#'
#' @family morphometry functions
#' @family gifti writers
#'
#' @examples
#'   label = c(1L, 23L, 44L); # the positive vertex indices
#'   outfile = tempfile(fileext=".gii");
#'   write.fs.label.gii(outfile, label, 50L);
#'
#' @export
write.fs.label.gii <- function(filepath, vertex_indices, num_vertices_in_surface) {
  if(! is.character(filepath)) {
    stop("Paramater 'filepath' must be a character string.");
  }
  if(max(vertex_indices) > num_vertices_in_surface) {
    stop("The maximal entry in 'vertex_indices' is larger than 'num_vertices_in_surface'.");
  }
  label = rep(0L, num_vertices_in_surface);
  label[vertex_indices] = 1L;
  xmltree = gifti_xml(list(label), intent='NIFTI_INTENT_LABEL', datatype='NIFTI_TYPE_INT32');
  xmltree = giftixml_add_labeltable_posneg(xmltree);
  gifti_xml_write(filepath, xmltree);
  return(invisible(label));
}

