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
#' @param format character string, the format to use. One of 'bin' for FreeSurfer binary surface format, 'asc' for FreeSurfer ASCII format, 'vtk' for VTK ASCII legacy format, 'ply' for Standford PLY format, 'off' for Object File Format, 'obj' for Wavefront object format, 'gii' for GIFTI format, 'mz3' for Surf-Ice MZ3 fomat, 'byu' for Brigham Young University (BYU) mesh format, or 'auto' to derive the format from the file extension given in parameter 'filepath'. With 'auto', a path ending in '.asc' is interpreted as 'asc', a path ending in '.vtk' as vtk, and so on for the other formats. Everything not matching any of these is interpreted as 'bin', i.e., FreeSurfer binary surface format.
#'
#' @return character string, the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh functions
#' @family mesh export functions
#'
#'
#' @examples
#' \dontrun{
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

  if(!(format %in% c('auto', 'bin', 'asc', 'vtk', 'obj', 'off', 'ply', 'gii', 'mz3', 'byu'))) {
    stop("Format must be one of c('auto', 'bin', 'asc', 'vtk', 'obj', 'off', 'ply', 'gii', 'mz3', 'byu').");
  }

  if(!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces);

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(write.fs.surface.asc(filepath, vertex_coords, faces));
  }

  if(format == 'vtk' | (format == 'auto' & filepath.ends.with(filepath, c('.vtk')))) {
    return(write.fs.surface.vtk(filepath, vertex_coords, faces));
  }

  if(format == 'obj' | (format == 'auto' & filepath.ends.with(filepath, c('.obj')))) {
    return(write.fs.surface.obj(filepath, vertex_coords, faces));
  }

  if(format == 'off' | (format == 'auto' & filepath.ends.with(filepath, c('.off')))) {
    return(write.fs.surface.off(filepath, vertex_coords, faces));
  }

  if(format == 'ply' | (format == 'auto' & filepath.ends.with(filepath, c('.ply')))) {
    return(write.fs.surface.ply(filepath, vertex_coords, faces));
  }

  if(format == 'gii' | (format == 'auto' & filepath.ends.with(filepath, c('.gii')))) {
    return(write.fs.surface.gii(filepath, vertex_coords, faces));
  }

  if(format == 'mz3' | (format == 'auto' & filepath.ends.with(filepath, c('.mz3')))) {
    return(write.fs.surface.mz3(filepath, vertex_coords, faces));
  }

  if(format == 'byu' | (format == 'auto' & filepath.ends.with(filepath, c('.byu')))) {
    return(write.fs.surface.byu(filepath, vertex_coords, faces));
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

  return(invisible(format_written));
}


#' @keywords internal
check.verts.faces <- function(vertex_coords, faces) {
  if(ncol(vertex_coords) != 3L) {
      stop("Parameter 'vertex_coords' must be a matrix with 3 columns (the x, y, z coords of the vertices).");
  }
  if(ncol(faces) != 3L) {
      stop("Parameter 'faces' must be a matrix with 3 columns (the indices of the vertices making up the faces).");
  }
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
#' \dontrun{
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

  check.verts.faces(vertex_coords, faces);

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
#' \dontrun{
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

  check.verts.faces(vertex_coords, faces);

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


#' @title Write mesh to file in Wavefront object (.obj) format
#'
#' @description The wavefront object format is a simply ASCII format for storing meshes.
#'
#' @param filepath string. Full path to the output surface file, should end with '.vtk', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @param vertex_colors vector of vertex colors. Will be written after the x, y, z coords on vertex lines. WARNING: This is NOT part of the official OBJ standard, and may not work with other software and even break some parsers.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh export functions
#'
#' @note Do not confuse the Wavefront object file format (.obj) with the OFF format (.off), they are not identical.
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.obj(tempfile(fileext=".obj"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.obj <- function(filepath, vertex_coords, faces, vertex_colors=NULL) {

  check.verts.faces(vertex_coords, faces);

  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);

  # Write the vertex data
  vs = matrix(rep('v', num_verts), ncol=1L);
  verts = cbind(vs, vertex_coords);

  use_vertex_colors = !is.null(vertex_colors);
  if(use_vertex_colors) {
    if(is.character(vertex_colors)) {
      vertex_colors = t(grDevices::col2rgb(vertex_colors, alpha = FALSE));
    }
    if((! is.integer(vertex_colors)) | ncol(vertex_colors) != 3L) {
      stop("Parameter 'vertex_colors' must be a matrix of integers with 3 columns (RGB) in range 0-255.");
    }
    vertex_data = data.frame(verts);
    vertex_colors_df = data.frame(vertex_colors/ 255.0);
    if(nrow(vertex_data) != nrow(vertex_colors_df)) {
      stop(sprintf("Data mismatch, received %d vertices but %d vertex colors.\n", nrow(vertex_data), nrow(vertex_colors_df)));
    }
    colnames(vertex_colors_df) = c('r', 'g', 'b');
    verts = cbind(vertex_data, vertex_colors_df);
  }

  write.table(verts, file = filepath, append = FALSE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  # Append the face data
  # Note that we do not shift the index, the format uses 1-based indices like R
  fs = matrix(rep('f', num_faces), ncol=1L);
  faces = cbind(fs, faces);
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  return(invisible('tris'));
}


#' @title Write mesh to file in Object File Format (.off)
#'
#' @description The Object File Format is a simply ASCII format for storing meshes.
#'
#' @param filepath string. Full path to the output surface file, should end with '.off', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @note Do not confuse the OFF format (.off) with the Wavefront object file format (.obj), they are not identical.
#'
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.off(tempfile(fileext=".off"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.off <- function(filepath, vertex_coords, faces) {
  return(write.fs.surface.off.ply2(filepath, vertex_coords, faces, format = 'off'));
}


#' @title Write mesh to file in Object File Format (.off) or PLY2 format.
#'
#' @description The two formats are very similar, they only differ in the header lines. This function can write both.
#'
#' @param filepath string. Full path to the output surface file, should end with '.off', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @param format character string, the format to write. One of 'ply2' or 'off'.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @note Do not confuse the OFF format (.off) with the Wavefront object file format (.obj), they are not identical.
#'
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.off(tempfile(fileext=".off"), mesh$vertices, mesh$faces);
#' }
#'
#' @keywords internal
write.fs.surface.off.ply2 <- function(filepath, vertex_coords, faces, format) {

  if(! format %in% c('ply2', 'off')) {
    stop("Format must be 'ply2' or 'off'.");
  }

  check.verts.faces(vertex_coords, faces);

  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);

  fh = file(filepath, "w");

  # write header
  if(format == 'off') {
    count_line = sprintf("%d %d %d", num_verts, num_faces, 0L);
    writeLines(c("# OFF", count_line), fh);
  } else {
    vertex_count_line = sprintf("%d", num_verts);
    face_count_line = sprintf("%d", num_faces);
    writeLines(c(vertex_count_line, face_count_line), fh);
  }
  close(fh);

  # Append the vertex data
  write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  # Append the face data
  faces = faces - 1L;   # shift index to 0-based
  faces = cbind(3L, faces);   # each line starts with the number of verts in the face
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  return(invisible('tris'));
}


#' @title Write mesh to file in PLY2 File Format (.ply2)
#'
#' @description The PLY2 file format is a simply ASCII format for storing meshes. It is very similar to OFF and by far not as flexible as PLY.
#'
#' @inheritParams write.fs.surface.off
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.ply2(tempfile(fileext=".ply2"), mesh$vertices, mesh$faces);
#' }
#'
#' @export
write.fs.surface.ply2 <- function(filepath, vertex_coords, faces) {
  return(write.fs.surface.off.ply2(filepath, vertex_coords, faces, format = 'ply2'));
}


#' @title Write mesh to file in PLY format (.ply)
#'
#' @description The PLY format is a versatile ASCII format for storing meshes. Also known as Polygon File Format or Stanford Triangle Format.
#'
#' @param filepath string. Full path to the output surface file, should end with '.vtk', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces m x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in FreeSurfer style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @param vertex_colors optional, matrix of RGBA vertex colors, number of rows must be the same as for vertex_coords. Color values must be integers in range 0-255. Alternatively, a vector of *n* RGB color strings can be passed.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @references See http://paulbourke.net/dataformats/ply/ for the PLY format spec.
#'
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.ply(tempfile(fileext=".ply"), mesh$vertices, mesh$faces);
#'
#'     # save a version with RGBA vertex colors
#'     vertex_colors = matrix(rep(82L, 5*4), ncol=4);
#'     write.fs.surface.ply(tempfile(fileext=".ply"), mesh$vertices,
#'      mesh$faces, vertex_colors=vertex_colors);
#' }
#'
#' @export
#' @importFrom grDevices col2rgb
write.fs.surface.ply <- function(filepath, vertex_coords, faces, vertex_colors=NULL) {

  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);

  check.verts.faces(vertex_coords, faces);

  fh = file(filepath, "w");

  use_vertex_colors = !is.null(vertex_colors);

  # write header
  header_lines = ply.header.lines(num_verts, num_faces, use_vertex_colors);
  writeLines(header_lines, fh);
  close(fh);

  # Append the vertex data
  if(use_vertex_colors) {
    if(is.character(vertex_colors)) {
      vertex_colors = t(grDevices::col2rgb(vertex_colors, alpha = TRUE));
    }
    if((! is.integer(vertex_colors)) | ncol(vertex_colors) != 4L) {
      stop("Parameter 'vertex_colors' must be a matrix of integers with 4 columns (RGBA) in range 0-255.");
    }
    vertex_data = data.frame(vertex_coords);
    vertex_colors_df = data.frame(vertex_colors);
    if(nrow(vertex_data) != nrow(vertex_colors_df)) {
      stop(sprintf("Data mismatch, received %d vertices but %d vertex colors.\n", nrow(vertex_data), nrow(vertex_colors_df)));
    }
    colnames(vertex_colors_df) = c('r', 'g', 'b', 'a');
    vertex_data = cbind(vertex_data, vertex_colors_df);
    write.table(vertex_data, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  } else {
    write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  }


  # Append the face data
  faces = faces - 1L;   # shift index to 0-based
  faces = cbind(3L, faces);   # each line starts with the number of verts in the face
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);

  return(invisible('tris'));
}


#' @title Generate PLY format header lines
#' @keywords internal
ply.header.lines <- function(num_verts, num_faces, use_vertex_colors) {

    header_top = c("ply", "format ascii 1.0");
    header_verts = c(sprintf("element vertex %d", num_verts), "property float x",  "property float y", "property float z");
    header_vertex_colors = c("property uchar red", "property uchar green", "property uchar blue", "property uchar alpha");

    header_faces = c(sprintf("element face %d", num_faces), "property list uchar int vertex_indices");
    header_end = "end_header";

    if(use_vertex_colors) {
      return(c(header_top, header_verts, header_vertex_colors, header_faces, header_end));
    } else {
      return(c(header_top, header_verts, header_faces, header_end));
    }
}



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
#' @family gifti writers
#'
#' @examples
#' \dontrun{
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
  if(!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces);
  my_data_sets = list(vertex_coords, faces - 1L);
  xmltree = gifti_xml(my_data_sets, datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32'), intent=c('NIFTI_INTENT_POINTSET', 'NIFTI_INTENT_TRIANGLE'));
  #xml2::xml_validate(xmltree, xml2::read_xml("https://www.nitrc.org/frs/download.php/158/gifti.xsd"));
  gifti_xml_write(filepath, xmltree);
  return(invisible('tris'));
}


#' @title Write mesh to file in mz3 binary format.
#'
#' @param filepath string. Full path to the output surface file, should end with '.mz3', but that is not enforced.
#'
#' @inheritParams write.fs.surface
#'
#' @param gzipped logical, whether to write a gzip compressed file
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.mz3(tempfile(fileext=".mz3"), mesh$vertices, mesh$faces);
#' }
#'
#' @note This format is used by the surf-ice renderer. The format spec is at https://github.com/neurolabusc/surf-ice/tree/master/mz3.
#'
#' @export
write.fs.surface.mz3 <- function(filepath, vertex_coords, faces, gzipped=TRUE) {

  if(!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces);

  faces = faces - 1L;

  format_written = "tris";
  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);
  num_skip = 0L;

  if(gzipped) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  magic_code = 23117L;
  attr = 3L; # verts + faces
  writeBin(as.integer(magic_code), fh, size = 2, endian = "little");
  writeBin(as.integer(attr), fh, size = 2, endian = "little");

  writeBin(as.integer(num_faces), fh, size = 4, endian = "little");
  writeBin(as.integer(num_verts), fh, size = 4, endian = "little");
  writeBin(as.integer(num_skip), fh, size = 4, endian = "little");

  # header done, now write the data itself.

  # write vertex indices making up a face
  writeBin(c(t(faces)), fh, size = 4, endian = "little");

  # write vertex coords
  writeBin(c(t(vertex_coords)), fh, size = 4, endian = "little");
  close(fh);

  return(invisible(format_written));
}


#' @title Write fixed width integers to one or several lines.
#'
#' @param vdata integer vector, the data
#'
#' @param num_chars_per_entry field length of a single formatted integer in characters
#'
#' @param max_entries_per_line integer, how many entries are allowed per line. Leave at NULL for no limit, which will return all in a single line.
#'
#' @param align_right logical, whether to align the integers to the right. As you may have guessed, set to `FALSE` to align to the left.
#'
#' @return vector of character strings, the formatted data lines.
#'
#' @keywords internal
fixed.vec.format.int <- function(vdata, num_chars_per_entry, max_entries_per_line=NULL, align_right=TRUE) {
  num_chars_per_entry = as.integer(num_chars_per_entry);
  if(align_right) {
    format_string = sprintf("%% %dd", num_chars_per_entry);
  } else {
    format_string = sprintf("%% -%dd", num_chars_per_entry);
  }
  return(fixed.format.lines(vdata, format_string, max_entries_per_line = max_entries_per_line));
}

#' @keywords internal
fixed.format.lines <- function(vdata, format_string, max_entries_per_line=NULL) {
  if(is.null(max_entries_per_line)) {
    return(paste(sprintf(format_string, vdata), collapse=""));
  } else {
    result_string = NULL;
    num_left = length(vdata);
    start_idx = 1L;
    while(num_left > 0L) {
      if(num_left >= max_entries_per_line) {
        end_idx = start_idx + max_entries_per_line - 1L;
      } else {
        end_idx = length(vdata);
      }
      num_written = end_idx - start_idx + 1L;
      this_line = paste(sprintf(format_string, vdata[start_idx:end_idx]), collapse="");
      result_string = c(result_string, this_line);
      num_left = num_left - num_written;
      start_idx = end_idx + 1L;
    }
    return(result_string);
  }
}


#' @title Write mesh to file in BYU ASCII format.
#'
#' @param filepath string. Full path to the output surface file, should end with '.byu', but that is not enforced.
#'
#' @inheritParams write.fs.surface
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh functions
#'
#' @examples
#' \dontrun{
#'     # Read a surface from a file:
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'      package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'
#'     # Now save it:
#'     write.fs.surface.byu(tempfile(fileext=".byu"), mesh$vertices, mesh$faces);
#' }
#'
#' @note This is a fixed field length ASCII format. Keep in mind that the BYU format expects the coordinates to be in the cube -1 to +1 on all three axes.
#'
#' @export
write.fs.surface.byu <- function(filepath, vertex_coords, faces) {

  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);

  check.verts.faces(vertex_coords, faces);

  # The BYU format expects the coordinates to be in the cube -1 to +1 on all axes. Warn if that is not the case.
  coord_min = min(min(vertex_coords[,1]), min(vertex_coords[,2]), min(vertex_coords[,3]));
  coord_max = max(max(vertex_coords[,1]), max(vertex_coords[,2]), max(vertex_coords[,3]));
  if(coord_min < -1.0 | coord_max > 1.0) {
     stop(sprintf("Exported BYU mesh contains vertex coordinates outside of expected range -1 to 1 (coord range of mesh is %f to %f). Consider rescaling.\n", coord_min, coord_max));
  }

  fh = file(filepath, "w");

  # write header
  num_meshes = 1L;
  header_data = c(num_meshes, num_verts, num_faces, (num_faces * 3L), 0L);
  header_line = fixed.vec.format.int(header_data, num_chars_per_entry=6L);
  # write the lines identifying the start and stop of the meshes in the faces list. Only one mesh in our case, that spans the entire list.
  part_line = fixed.vec.format.int(c(1L, (num_faces * 3L)), num_chars_per_entry=6L);
  writeLines(c(header_line, part_line), fh);

  # write the vertex coordinates
  vertex_coords_vec = as.double(t(vertex_coords));
  vertex_lines = fixed.format.lines(vertex_coords_vec, format_string="% 1.5e", max_entries_per_line=6L);
  writeLines(vertex_lines, fh);

  # Write faces.
  # Turn matrix of vertex indices into a vector and make the index of the last vertex of each face negative:
  face_vertex_indices = as.integer(t(faces));
  last_vertices_of_faces_indices = seq.int(3L, length(face_vertex_indices), by=3L);
  face_vertex_indices[last_vertices_of_faces_indices] = -face_vertex_indices[last_vertices_of_faces_indices];
  face_lines = fixed.vec.format.int(face_vertex_indices, num_chars_per_entry=6L, max_entries_per_line=16L);
  writeLines(face_lines, fh);

  close(fh);
}


#' @title Write surface to Brainvoyager SRF file.
#'
#' @inheritParams write.fs.surface
#'
#' @param normals matrix of nx3 vertex normals (x,y,z)
#'
#' @param neighborhoods list of integer lists, the indices of the nearest neighbors for each vertex (an adjacency list). The sub list at index n contains the indices of the vertices in the 1-neighborhood of vertex n. The vertex indices in the sub lists must be zero-based.
#'
#' @note This function is experimental. Only SRF file format version 4 is supported.
#'
#' @export
write.fs.surface.bvsrf <- function(filepath, vertex_coords, faces, normals=NULL, neighborhoods=NULL) {
  num_verts = nrow(vertex_coords);
  num_faces = nrow(faces);
  endian = 'little';

  check.verts.faces(vertex_coords, faces);

  if(is.null(normals)) {
    normals = rep(0.0, (num_verts * 3L));
    normals = matrix(normals, ncol = 3);
  }

  fh = file(filepath, "wb");
  writeBin(as.double(4.0), fh, size = 4, endian = endian); # SRF file format version
  writeBin(as.integer(0L), fh, size = 4, endian = endian);  # must be 0
  writeBin(as.integer(num_verts), fh, size = 4, endian = endian);
  writeBin(as.integer(num_faces), fh, size = 4, endian = endian);

  writeBin(as.double(128.0), fh, size = 4, endian = endian); # next 3 are mesh center x,y,z
  writeBin(as.double(128.0), fh, size = 4, endian = endian);
  writeBin(as.double(128.0), fh, size = 4, endian = endian);

  writeBin(as.double(vertex_coords[,1]), fh, size = 4, endian = endian); # vert coord x, z, y
  writeBin(as.double(vertex_coords[,2]), fh, size = 4, endian = endian);
  writeBin(as.double(vertex_coords[,3]), fh, size = 4, endian = endian);

  writeBin(as.double(normals[,1]), fh, size = 4, endian = endian); # vert normals x, z, y
  writeBin(as.double(normals[,2]), fh, size = 4, endian = endian);
  writeBin(as.double(normals[,3]), fh, size = 4, endian = endian);

  writeBin(as.double(0.322), fh, size = 4, endian = endian); # convex vert color RGBA
  writeBin(as.double(0.733), fh, size = 4, endian = endian);
  writeBin(as.double(0.980), fh, size = 4, endian = endian);
  writeBin(as.double(1.000), fh, size = 4, endian = endian);

  writeBin(as.double(0.100), fh, size = 4, endian = endian); # concave vert color RGBA
  writeBin(as.double(0.240), fh, size = 4, endian = endian);
  writeBin(as.double(0.320), fh, size = 4, endian = endian);
  writeBin(as.double(1.000), fh, size = 4, endian = endian);

  mesh_col = rep(0L, num_verts);
  writeBin(as.integer(mesh_col), fh, size = 4, endian = endian);

  # nearest neighbor data
  if(is.null(neighborhoods)) {
    neighborhood_sizes = rep(0L, num_verts); # we do not have neighborhood data
    writeBin(as.integer(neighborhood_sizes), fh, size = 4, endian = endian);
  } else {
    for(neighbors in neighborhoods) {
      writeBin(as.integer(length(neighbors)), fh, size = 4, endian = endian);
      writeBin(as.integer(neighbors), fh, size = 4, endian = endian);
    }
  }

  # writes faces
  faces = faces - 1L;
  writeBin(as.integer(t(faces)), fh, size = 4, endian = endian);
  writeBin(as.integer(0L), fh, size = 4, endian = endian); # num triangle strips
  writeChar("", fh); # associated file name
  close(fh);
}

# lh = freesurferformats::read.fs.surface('~/data/tim_only/tim/surf/lh.white')
# cm = fsbrain::coloredmesh.from.morph.native('~/data/tim_only', 'tim', 'sulc', hemi='lh')
# freesurferformats::write.fs.surface.obj("~/lh.obj", lh$vertices, lh$faces, vertex_colors=cm$col)
# library('rayrender')
# bscene = generate_ground() %>% add_object(rayrender::obj_model("~/lh.obj", x= 20, y = 200, z = 20, material = diffuse(sigma=90), vertex_colors = TRUE))
# render_scene(bscene, parallel = TRUE, width = 800, height = 800, samples = 1000, lookfrom = c(-550, 160, 0), lookat = c(0, 180, 80))
