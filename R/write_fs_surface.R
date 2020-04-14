#' @title Write mesh to file in FreeSurfer binary surface format
#'
#' @description Write vertex coordinates and vertex indices defining faces to a file in FreeSurfer binary surface format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'. This function writes the triangle version of the surface file format.
#'
#' @param filepath string. Full path to the output curv file. If it ends with ".gz", the file is written in gzipped format. Note that this is not common, and that other software may not handle this transparently.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @param format character string, the format to use. One of 'bin' for FreeSurfer binary surface format, 'asc' for FreeSurfer ASCII format, 'vtk' for VTK ASCII legacy format, or 'auto' to derive the format from the file extension given in parameter 'filepath'. With 'auto', a path ending in '.asc' is interpreted as 'asc', a path ending in '.vtk' as vtk, and everything else as 'bin'.
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
#'     write.fs.surface(tempfile(), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface <- function(filepath, vertex_coords, faces, format='auto') {

  if(!(format %in% c('auto', 'bin', 'asc', 'vtk'))) {
    stop("Format must be one of c('auto', 'bin', 'asc', 'vtk').");
  }

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(write.fs.surface.asc(filepath, vertex_coords, faces));
  }

  if(format == 'vtk' | (format == 'auto' & filepath.ends.with(filepath, c('.vtk')))) {
    return(write.fs.surface.vtk(filepath, vertex_coords, faces));
  }

  TRIS_MAGIC_FILE_TYPE_NUMBER = 16777214L;
  OLD_QUAD_MAGIC_FILE_TYPE_NUMBER = 16777215L;
  NEW_QUAD_MAGIC_FILE_TYPE_NUMBER = 16777213L;

  num_faces_with_index_zero = sum(faces==0);
  if(num_faces_with_index_zero > 0) {
    stop(sprintf("The vertex indices defining the faces must be 1-based (GNU R style). That means the value 0 must not occur in the matrix 'faces', but %d of the %d face vertex indices have this value (most likely you will need to add 1 to all values in 'faces').", num_faces_with_index_zero, length(faces)));
  }

  faces = faces - 1L;

  if(typeof(faces) != "integer") {
    stop(sprintf("The type of the faces matrix must be 'integer' but is '%s'.", typeof(faces)));
  }

  if(ncol(faces) == 3) {
    MAGIC_FILE_TYPE_NUMBER = TRIS_MAGIC_FILE_TYPE_NUMBER;
    format_written = "tris";
    num_verts = nrow(vertex_coords);
    num_faces = nrow(faces);

    if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
      fh = gzfile(filepath, "wb");
    } else {
      fh = file(filepath, "wb", blocking = TRUE);
    }

    fwrite3(fh, MAGIC_FILE_TYPE_NUMBER);

    creation_date_line = "Created by anonymous on a perfect day.\n";
    writeLines(creation_date_line, con=fh);

    writeBin(as.integer(num_verts), fh, size = 4, endian = "big");
    writeBin(as.integer(num_faces), fh, size = 4, endian = "big");

    # write the data itself: vertex coords
    writeBin(c(t(vertex_coords)), fh, size = 4, endian = "big");

    # write vertex indices making up a face
    writeBin(c(t(faces)), fh, size = 4, endian = "big");
    close(fh);
  } else if (ncol(faces) == 4) {
    MAGIC_FILE_TYPE_NUMBER = OLD_QUAD_MAGIC_FILE_TYPE_NUMBER;
    format_written = "quads";
    stop("Sorry, writing QUAD files not implemented yet. Use TRIS instead (3 vertex indices per face instead of 4).")
  } else {
    format_written = NULL;
    stop(sprintf("Each face must be made up of exactly 3 vertices (for triangular meshes) or 4 vertices (for quads), but found %d columns in matrix 'faces'.", ncol(faces)));
  }
  return(invisible(format_written));
}


#' @title Write mesh to file in FreeSurfer ASCII surface format
#'
#' @description Write vertex coordinates and vertex indices defining faces to a file in FreeSurfer ASCII surface format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white.asc'.
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
#'     write.fs.surface.asc(tempfile(fileext=".asc"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.asc <- function(filepath, vertex_coords, faces) {
  # Write the first comment line and the 2nd line containing the number of vertices in the label
  fh =  file(filepath);
  writeLines(c("#!ascii version of surface", sprintf("%d %d", nrow(vertex_coords), nrow(faces))), fh);
  close(fh);

  if(ncol(vertex_coords) == 3) {
    vertex_coords = cbind(vertex_coords, 0L);   # add isInPatch flag (can be 0 or 1), aka 'ripflag'
  }

  faces = faces - 1L;
  if(ncol(faces) == 3) {
    faces = cbind(faces, 0L);   # add isInPatch flag (can be 0 or 1)
  }

  # Append the vertex data
  write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  # Append the face data
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  return(invisible('tris'));
}


#' @title Write mesh to file in VTK ASCII format
#'
#' @param filepath string. Full path to the output surface file, should end with '.vtk', but that is not enforced.
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
#'     write.fs.surface.vtk(tempfile(fileext=".vtk"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.vtk <- function(filepath, vertex_coords, faces) {

  fh = file(filepath, "w");

  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);

  # write header
  writeLines(c("# vtk DataFile Version 1.0", "fsbrain output", "ASCII", "DATASET POLYDATA", sprintf("POINTS %d float", num_verts)), fh);
  close(fh);


  # Append the vertex data
  write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  fh = file(filepath, "a");
  writeLines(c(sprintf("POLYGONS %d %d", num_faces, num_faces * 4L)), fh);
  close(fh);

  # Append the face data

  faces = faces - 1L;       # from R to 0-based indices
  faces = cbind(3L, faces); # in VTK format, each face line starts with the number of vertices
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  return(invisible('tris'));
}
