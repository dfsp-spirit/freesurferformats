
#' @title Read FreeSurfer ASCII format surface.
#'
#' @param filepath string. Full path to the input surface file in ASCII surface format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.asc <- function(filepath) {

  num_verts_and_faces_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts', 'num_faces'), colClasses = c("integer", "integer"));
  num_verts = num_verts_and_faces_df$num_verts[1];
  num_faces = num_verts_and_faces_df$num_faces[1];

  vertices_df = read.table(filepath, skip=2L, col.names = c('coord1', 'coord2', 'coord3', 'value'), colClasses = c("numeric", "numeric", "numeric", "numeric"), nrows=num_verts);

  faces_df = read.table(filepath, skip=2L + num_verts, col.names = c('vertex1', 'vertex2', 'vertex3', 'value'), colClasses = c("integer", "integer", "integer", "numeric"), nrows=num_faces);

  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[1:3]));
  ret_list$faces = unname(data.matrix(faces_df[1:3])) + 1L;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ASCII surface file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in ASCII surface file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));
  }

  return(ret_list);
}


#' @title Read VTK ASCII format mesh as surface.
#'
#' @description This reads meshes (vtk polygon datasets) from text files in VTK ASCII format. See \url{https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf} for format spec. Note that this function does **not** read arbitrary VTK datasets, i.e., it supports only a subset of the possible contents of VTK files (i.e., polygon meshes).
#'
#' @param filepath string. Full path to the input surface file in VTK ASCII format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is by far not a complete VTK format reader.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.vtk <- function(filepath) {

  all_lines = readLines(filepath);
  if(length(all_lines) < 5L) {
    stop("The file is not a valid VTK ASCII file: it does not contain the 4 header lines and a data line.");
  }
  if(! startsWith(all_lines[1], "# vtk DataFile Version")) {
    stop("The file is not a valid VTK ASCII file: first line is not a proper VTK file version descriptor.");
  }
  # line 2 is a freeform description, we do not check it
  if(all_lines[3] !=  "ASCII") {
    stop("The file is not a valid VTK ASCII file: third line does not read 'ASCII'.");
  }
  if(all_lines[4] !=  "DATASET POLYDATA") {
    stop("The file is not a VTK ASCII mesh file: forth line does not read 'DATASET POLYDATA'. Only mesh data is supported by this function.");
  }

  # Starting from here, several data sections may follow.
  vertices_df = NULL;
  faces_df = NULL;

  current_line_idx = 5L;
  while(current_line_idx <= length(all_lines)) {
    data_section_header_line_words = strsplit(all_lines[current_line_idx], " ")[[1]];
    data_type = data_section_header_line_words[1];
    num_elements = as.integer(data_section_header_line_words[2]);
    if(data_type == "POINTS") {
      vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('coord1', 'coord2', 'coord3'), colClasses = c("numeric", "numeric", "numeric"), nrows=num_elements);
    } else if(data_type == "POLYGONS") {
      faces_df = read.table(filepath, skip=current_line_idx, col.names = c('num_verts', 'vertex1', 'vertex2', 'vertex3'), colClasses = c("integer", "integer", "integer", "integer"), nrows=num_elements);
    } else {
      warning(sprintf("Unsupported data type in section staring at line %d: '%s'. Only 'POINTS' and 'POLYGONS' are supported. Skipping section.\n", current_line_idx, data_type));
    }
    current_line_idx = current_line_idx + num_elements + 1L;   # the +1L is for the section header line
  }


  if(is.null(vertices_df) | is.null(faces_df)) {
    stop("VTK file did not contain a complete mesh dataset (POINTS and POLYGONS sections).");
  }

  if(any(faces_df$num_verts != 3L)) {
    stop("The mesh in the VTK file contains POLYGONS with are not triangles. Only triangular meshes are supported by this function.");
  }


  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[1:3]));
  ret_list$faces = unname(data.matrix(faces_df[2:4])) + 1L;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.surface", class(ret_list));

  return(ret_list);
}


#' @title Read Stanford PLY format mesh as surface.
#'
#' @description This reads meshes from text files in PLY format. Note that this does not read arbitrary data from PLY files, i.e., PLY files can store data that is not supported by this function.
#'
#' @param filepath string. Full path to the input surface file in Stanford Triangle (PLY) format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#'
#' @note This is by far not a complete PLY format reader. It can read PLY mesh files which were written by \code{\link[freesurferformats]{write.fs.surface.ply}} and Blender. Vertex colors and Blender vertex normals are currently ignored (but files with them are supported in the sense that the mesh data will be read correctly).
#'
#' @export
read.fs.surface.ply <- function(filepath) {

  ply_lines = readLines(filepath);
  header_info = read.element.counts.ply.header(ply_lines);

  num_verts = header_info$num_verts;
  num_faces = header_info$num_faces;
  header_end_line_index = header_info$header_end_line_index;
  contains_vertex_colors = header_info$contains_vertex_colors;
  contains_vertex_normals = header_info$contains_vertex_normals;

  vertices_df = NULL;
  faces_df = NULL;

  current_line_idx = header_end_line_index;
  if(contains_vertex_colors & contains_vertex_normals) {
    vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('coord1', 'coord2', 'coord3', 'nx', 'ny', 'nz', 'r', 'g', 'b', 'a'), colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer"), nrows=num_verts);
  } else if(contains_vertex_colors) {
    vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('coord1', 'coord2', 'coord3', 'r', 'g', 'b', 'a'), colClasses = c("numeric", "numeric", "numeric", "integer", "integer", "integer", "integer"), nrows=num_verts);
  } else if(contains_vertex_normals) {
    vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('coord1', 'coord2', 'coord3', 'nx', 'ny', 'nz'), colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), nrows=num_verts);
  } else {
    vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('coord1', 'coord2', 'coord3'), colClasses = c("numeric", "numeric", "numeric"), nrows=num_verts);
  }

  current_line_idx = current_line_idx + nrow(vertices_df);
  faces_df = read.table(filepath, skip=current_line_idx, col.names = c('num_verts', 'vertex1', 'vertex2', 'vertex3'), colClasses = c("integer", "integer", "integer", "integer"), nrows=num_faces);

  current_line_idx = current_line_idx + nrow(faces_df);

  if(current_line_idx != length(ply_lines)) {
    warning(sprintf("At line %d after reading vertices and faces, but PLY file has %d lines. Ignored remaining lines.\n", current_line_idx, length(ply_lines)));
  }

  if(is.null(vertices_df) | is.null(faces_df)) {
    stop("PLY file did not contain a complete mesh dataset (vertices or faces missing).");
  }

  if(any(faces_df$num_verts != 3L)) {
    stop("The mesh in the PLY file contains POLYGONS with are not triangles. Only triangular meshes are supported by this function.");
  }


  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[1:3]));
  ret_list$faces = unname(data.matrix(faces_df[2:4])) + 1L;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.surface", class(ret_list));

  return(ret_list);
}


#' @title Determine element counts from PLY file header.
#'
#' @param ply_lines vector character strings, all lines of the PLY file
#'
#' @keywords internal
read.element.counts.ply.header <- function(ply_lines) {
  if(length(ply_lines) < 9L) {
    stop("The file is not a valid PLY mesh file: it does not contain the 9 header lines.");
    # 9 lines are required for the mandatory header fields and the lines defining the vertex and face properties.
  }
  if(ply_lines[1] != "ply") {
    stop("The file is not a valid PLY file: first line does not read 'ply'.");
  }
  if(ply_lines[2] != "format ascii 1.0") {
    stop("The file is not a valid PLY file in supported format: second line does not read 'format ascii 1.0'.");
  }

  if(length(which(ply_lines == "end_header")) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find header termination string.");
  } else {
    header_end_line_index = which(ply_lines == "end_header");
  }

  if(length(ply_lines) == header_end_line_index) {
    stop("PLY file contains no data elements.");
  }

  header_lines = ply_lines[1L:header_end_line_index];

  vertex_count_line_index = which(startsWith(header_lines, "element vertex"));
  if(length(vertex_count_line_index) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find vertex count header line.");
  }
  vertex_count_line_words = strsplit(ply_lines[vertex_count_line_index], " ")[[1]];
  vertex_count = as.integer(vertex_count_line_words[3]);


  face_count_line_index = which(startsWith(header_lines, "element face"));
  if(length(face_count_line_index) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find face count header line.");
  }
  face_count_line_words = strsplit(ply_lines[face_count_line_index], " ")[[1]];
  face_count = as.integer(face_count_line_words[3]);

  file_contains_vertex_colors = length(which(header_lines == "property uchar red")) == 1L;
  file_contains_vertex_normals = length(which(header_lines == "property float nx")) == 1L; # vertex normals as exported by Blender

  return(list('header_end_line_index'=header_end_line_index, 'num_verts'=vertex_count, 'num_faces'=face_count, 'contains_vertex_colors'=file_contains_vertex_colors, 'contains_vertex_normals'=file_contains_vertex_normals));
}


#' @title Read file in FreeSurfer surface format
#'
#' @description Read a brain surface mesh consisting of vertex and face data from a file in FreeSurfer binary or ASCII surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'.
#'
#' @param filepath string. Full path to the input surface file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @param format one of 'auto', 'asc', 'vtk' or 'bin'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.asc'.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. This datastructure is known as a is a *face index set*. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#'
#' @examples
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'     cat(sprintf("Read data for %d vertices and %d faces. \n",
#'                             nrow(mesh$vertices), nrow(mesh$faces)));
#'
#' @export
read.fs.surface <- function(filepath, format='auto') {

  if(!(format %in% c('auto', 'bin', 'asc', 'vtk'))) {
    stop("Format must be one of c('auto', 'bin', 'asc').");
  }

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(read.fs.surface.asc(filepath));
  }

  if(format == 'vtk' | (format == 'auto' & filepath.ends.with(filepath, c('.vtk')))) {
    return(read.fs.surface.vtk(filepath));
  }

  TRIS_MAGIC_FILE_TYPE_NUMBER = 16777214L;
  OLD_QUAD_MAGIC_FILE_TYPE_NUMBER = 16777215L;
  NEW_QUAD_MAGIC_FILE_TYPE_NUMBER = 16777213L;


  if(guess.filename.is.gzipped(filepath)) {
    fh = gzfile(filepath, "rb");
  } else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();

  magic_byte = fread3(fh);
  if (magic_byte == OLD_QUAD_MAGIC_FILE_TYPE_NUMBER | magic_byte == NEW_QUAD_MAGIC_FILE_TYPE_NUMBER) {
    warning("Reading QUAD files in untested atm. Please use with care. This warning will be removed once we have an example input file and the code has unit tests.")
    ret_list$mesh_face_type = "quads";

    num_vertices = fread3(fh);
    num_quad_faces = fread3(fh);    # These are QUAD faces
    num_tris_faces = num_quad_faces * 2L;   # There are twice as many tris faces
    cat(sprintf("Reading quad surface file, expecting %d vertices and %d quad faces.\n", num_vertices, num_quad_faces));

    ret_list$internal = list();
    ret_list$internal$num_vertices_expected = num_vertices;
    ret_list$internal$num_faces_expected = num_quad_faces;


    num_vertex_coords = num_vertices * 3L;
    if (magic_byte == OLD_QUAD_MAGIC_FILE_TYPE_NUMBER) {
      vertex_coords = readBin(fh, integer(), size=2L, n = num_vertex_coords, endian = "big");
      vertex_coords = vertex_coords / 100.;
    } else {
      # NEW_QUAD_MAGIC_FILE_TYPE_NUMBER
      vertex_coords = readBin(fh, numeric(), size=4L, n = num_vertex_coords, endian = "big");
    }
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L, byrow = TRUE);

    if(length(vertex_coords) != num_vertex_coords) {
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));
    }

    num_face_vertex_indices = num_quad_faces * 4L;
    face_vertex_indices = rep(0, num_face_vertex_indices);
    quad_faces = matrix(face_vertex_indices, nrow=num_quad_faces, ncol=4L, byrow = TRUE);
    for (face_idx in 1L:num_quad_faces) {
      for (vertex_idx_in_face in 1L:4L) {
        global_vertex_idx = fread3(fh);
        quad_faces[face_idx, vertex_idx_in_face] = global_vertex_idx;
      }
    }
    ret_list$internal$quad_faces = quad_faces;

    # Compute the tris-faces from the quad faces:
    faces = faces.quad.to.tris(quad_faces);


  } else if(magic_byte == TRIS_MAGIC_FILE_TYPE_NUMBER) {
    ret_list$mesh_face_type = "tris";

    creation_date_text_line = readBin(fh, character(), endian = "big");
    #cat(sprintf("creation_date_text_line= '%s'\n", creation_date_text_line))
    seek(fh, where=3, origin="current")
    info_text_line = readBin(fh, character(), endian = "big");
    #cat(sprintf("info_text_line= '%s'\n", info_text_line));
    seek(fh, where=-5, origin="current") # skip string termination

    ret_list$internal = list();
    ret_list$internal$creation_date_text_line = creation_date_text_line;
    ret_list$internal$info_text_line = info_text_line;

    #cur_pos = seek(fh, where=NA);
    #cat(sprintf("At position %d before reading num_vertices.\n", cur_pos));

    num_vertices = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    num_faces = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    ret_list$internal$num_vertices_expected = num_vertices;
    ret_list$internal$num_faces_expected = num_faces;

    num_vertex_coords = num_vertices * 3L;
    vertex_coords = readBin(fh, numeric(), size = 4L, n = num_vertex_coords, endian = "big");          # a vertex is made up of 3 float coordinates (x,y,z)
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L, byrow = TRUE);

    if(length(vertex_coords) != num_vertex_coords) {
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));
    }

    num_face_vertex_indices = num_faces * 3L;
    face_vertex_indices = readBin(fh, integer(), size = 4L, n = num_face_vertex_indices, endian = "big");   # a face is made of of 3 integers, which are vertex indices
    faces = matrix(face_vertex_indices, nrow=num_faces, ncol=3L, byrow = TRUE);
    faces = faces + 1L;    # Increment indices by 1: GNU R uses 1-based indices.

    if(length(face_vertex_indices) != num_face_vertex_indices) {
      stop(sprintf("Mismatch in read vertex indices for faces: expected %d but received %d.\n", num_face_vertex_indices, length(face_vertex_indices)));
    }

  } else {
    stop(sprintf("Magic number mismatch (%d != (%d || %d)). The given file '%s' is not a valid FreeSurfer surface format file in binary format. (Hint: This function is designed to read files like 'lh.white' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, TRIS_MAGIC_FILE_TYPE_NUMBER, NEW_QUAD_MAGIC_FILE_TYPE_NUMBER, filepath));
  }


  ret_list$vertices = vertices;
  ret_list$faces = faces;
  class(ret_list) = c("fs.surface", class(ret_list));
  return(ret_list);
}


#' @title Print description of a brain surface.
#'
#' @param x brain surface with class `fs.surface`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.surface <- function(x, ...) {
  cat(sprintf("Brain surface trimesh with %d vertices and %d faces.\n", nrow(x$vertices), nrow(x$faces)));
  cat(sprintf("-Surface coordinates: minimal values are (%.2f, %.2f, %.2f), maximal values are (%.2f, %.2f, %.2f).\n", min(x$vertices[,1]), min(x$vertices[,2]), min(x$vertices[,3]), max(x$vertices[,1]), max(x$vertices[,2]), max(x$vertices[,3])));
}


#' Convert quad faces to tris faces.
#'
#' @param quad_faces nx4 integer matrix, the indices of the vertices making up the *n* quad faces
#'
#' @return *2nx3* integer matrix, the indices of the vertices making up the *2n* tris faces
#'
#' @keywords internal
faces.quad.to.tris <- function(quad_faces) {
  num_quad_faces = nrow(quad_faces);
  num_tris_faces = num_quad_faces * 2L;
  tris_faces = matrix(rep(0L, num_tris_faces*3L), nrow=num_tris_faces, ncol=3L);
  for (quad_face_idx in 1L:num_quad_faces) {
    tris_face_2_index = quad_face_idx * 2L;
    tris_face_1_index = tris_face_2_index - 1L;
    tris_faces[tris_face_1_index,] = quad_faces[quad_face_idx, c(1,2,3)];  # c(1,2,4)];
    tris_faces[tris_face_2_index,] = quad_faces[quad_face_idx, c(3,4,1)];  # c(3,4,2)];
  }
  return(tris_faces);
}


#' @title Convert tris faces to quad faces by simple merging.
#'
#' @description This is experimental. Note that it can only work if the number of 'tris_faces' is even, as two consecutive tris-faces will be merged into one quad face. We could set the index to NA in that case, but I do not know how FreeSurfer handles this, so we do not guess.
#'
#' @param tris_faces *nx3* integer matrix, the indices of the vertices making up the *n* tris faces.
#'
#' @return n/2x4 integer matrix, the indices of the vertices making up the *n* quad faces.
#'
#' @note This function does not implement proper remeshing of tri-meshes to quad-meshes. Use a proper mesh library if you need that.
#'
#' @keywords internal
faces.tris.to.quad <- function(tris_faces) {
  num_tris_faces = nrow(tris_faces);
  if(num_tris_faces %% 2 != 0L) {
    stop("Number of tris faces must be even.");
  }
  num_quad_faces = num_tris_faces / 2L;

  quad_faces = matrix(rep(0L, num_quad_faces*4L), nrow=num_quad_faces, ncol=4L);
  for (tris_face_idx in seq.int(1L, num_tris_faces, 2L)) {
    tris_face_1_index = tris_face_idx;
    tris_face_2_index = tris_face_idx + 1L;
    quad_face_index = tris_face_2_index / 2L;

    quad_faces[quad_face_index,] = c(tris_faces[tris_face_1_index, c(1,2,3)], tris_faces[tris_face_2_index, 2]);
  }
  return(quad_faces);
}


#' @title Check whether object is an fs.surface
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain surface (that is, has "fs.surface" amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.surface <- function(x) inherits(x, "fs.surface")


