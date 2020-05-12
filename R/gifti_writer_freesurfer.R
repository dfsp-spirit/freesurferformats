

#' @title Write mesh to file in GIFTI surface format
#'
#' @description Write vertex coordinates and vertex indices defining faces to a file in GIFTI surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white.asc'.
#'
#' @param filepath string. Full path to the output surface file, should end with '.asc', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh functions
#'
#' @examples
#' \donttest{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.gii(tempfile(fileext=".gii"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.gii <- function(filepath, vertex_coords, faces) {
  my_data_sets = list(vertex_coords, faces - 1L);
  xmltree = gifti_xml(my_data_sets, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), intent=c('NIFTI_INTENT_POINTSET', 'NIFTI_INTENT_TRIANGLE'));
  xml2::xml_validate(xmltree, xml2::read_xml("https://www.nitrc.org/frs/download.php/158/gifti.xsd"));
  gifti_xml_write(filepath, xmltree);
  return(invisible('tris'));
}
