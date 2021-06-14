
#' @title Read FreeSurfer ASCII format surface.
#'
#' @param filepath string. Full path to the input surface file in ASCII surface format.
#'
#' @param with_values logical, whether to read per-vertex and per-face values.
#'
#' @param header_numlines scalar positive integer, the number of header lines.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is also known as *srf* format.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.asc <- function(filepath, with_values = TRUE, header_numlines = 2L) {

  num_verts_and_faces_df = read.table(filepath, skip=0L, nrows=1L, col.names = c('num_verts', 'num_faces'), colClasses = c("integer", "integer"));
  num_verts = num_verts_and_faces_df$num_verts[1];
  num_faces = num_verts_and_faces_df$num_faces[1];

  if(with_values) {
    vertices_df = read.table(filepath, skip=header_numlines, col.names = c('coord1', 'coord2', 'coord3', 'value'), colClasses = c("numeric", "numeric", "numeric", "numeric"), nrows=num_verts);
    faces_df = read.table(filepath, skip=header_numlines + num_verts, col.names = c('vertex1', 'vertex2', 'vertex3', 'value'), colClasses = c("integer", "integer", "integer", "numeric"), nrows=num_faces);
  } else {
    vertices_df = read.table(filepath, skip=header_numlines, col.names = c('coord1', 'coord2', 'coord3'), colClasses = c("numeric", "numeric", "numeric"), nrows=num_verts);
    faces_df = read.table(filepath, skip=header_numlines + num_verts, col.names = c('vertex1', 'vertex2', 'vertex3'), colClasses = c("integer", "integer", "integer"), nrows=num_faces);
  }

  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[1:3]));
  ret_list$faces = unname(data.matrix(faces_df[1:3])) + 1L;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ASCII surface file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices))); # nocov
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in ASCII surface file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces))); # nocov
  }

  return(ret_list);
}


#' @title Read VTK ASCII format mesh as surface.
#'
#' @description This reads meshes (vtk polygon datasets) from text files in VTK ASCII format. See https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf for format spec. Note that this function does **not** read arbitrary VTK datasets, i.e., it supports only a subset of the possible contents of VTK files (i.e., polygon meshes).
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
    stop("The file is not a valid VTK ASCII file: it does not contain the 4 header lines and a data line."); # nocov
  }
  if(! startsWith(all_lines[1], "# vtk DataFile Version")) {
    stop("The file is not a valid VTK ASCII file: first line is not a proper VTK file version descriptor."); # nocov
  }
  # line 2 is a freeform description, we do not check it
  if(all_lines[3] !=  "ASCII") {
    stop("The file is not a valid VTK ASCII file: third line does not read 'ASCII'."); # nocov
  }
  if(all_lines[4] !=  "DATASET POLYDATA") {
    stop("The file is not a VTK ASCII mesh file: forth line does not read 'DATASET POLYDATA'. Only mesh data is supported by this function."); # nocov
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
      warning(sprintf("Unsupported data type in section staring at line %d: '%s'. Only 'POINTS' and 'POLYGONS' are supported. Skipping section.\n", current_line_idx, data_type)); # nocov
    }
    current_line_idx = current_line_idx + num_elements + 1L;   # the +1L is for the section header line
  }


  if(is.null(vertices_df) | is.null(faces_df)) {
    stop("VTK file did not contain a complete mesh dataset (POINTS and POLYGONS sections)."); #nocov
  }

  if(any(faces_df$num_verts != 3L)) {
    stop("The mesh in the VTK file contains POLYGONS which are not triangles. Only triangular meshes are supported by this function."); # nocov
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
    warning(sprintf("At line %d after reading vertices and faces, but PLY file has %d lines. Ignored remaining lines.\n", current_line_idx, length(ply_lines))); # nocov
  }

  if(is.null(vertices_df) | is.null(faces_df)) {
    stop("PLY file did not contain a complete mesh dataset (vertices or faces missing)."); # nocov
  }

  if(any(faces_df$num_verts != 3L)) {
    stop("The mesh in the PLY file contains faces which are not triangles. Only triangular meshes are supported by this function."); # nocov
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
    stop("The file is not a valid PLY mesh file: it does not contain the 9 header lines."); # nocov
    # 9 lines are required for the mandatory header fields and the lines defining the vertex and face properties.
  }
  if(ply_lines[1] != "ply") {
    stop("The file is not a valid PLY file: first line does not read 'ply'."); # nocov
  }
  if(ply_lines[2] != "format ascii 1.0") {
    stop("The file is not a valid PLY file in supported format: second line does not read 'format ascii 1.0'.");  # nocov
  }

  if(length(which(ply_lines == "end_header")) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find header termination string.");  # nocov
  } else {
    header_end_line_index = which(ply_lines == "end_header");
  }

  if(length(ply_lines) == header_end_line_index) {
    stop("PLY file contains no data elements.");  # nocov
  }

  header_lines = ply_lines[1L:header_end_line_index];

  vertex_count_line_index = which(startsWith(header_lines, "element vertex"));
  if(length(vertex_count_line_index) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find vertex count header line.");  # nocov
  }
  vertex_count_line_words = strsplit(ply_lines[vertex_count_line_index], " ")[[1]];
  vertex_count = as.integer(vertex_count_line_words[3]);


  face_count_line_index = which(startsWith(header_lines, "element face"));
  if(length(face_count_line_index) != 1L) {
    stop("The file is not a valid PLY file in supported format: could not find face count header line.");  # nocov
  }
  face_count_line_words = strsplit(ply_lines[face_count_line_index], " ")[[1]];
  face_count = as.integer(face_count_line_words[3]);

  file_contains_vertex_colors = length(which(header_lines == "property uchar red")) == 1L;
  file_contains_vertex_normals = length(which(header_lines == "property float nx")) == 1L; # vertex normals as exported by Blender

  return(list('header_end_line_index'=header_end_line_index, 'num_verts'=vertex_count, 'num_faces'=face_count, 'contains_vertex_colors'=file_contains_vertex_colors, 'contains_vertex_normals'=file_contains_vertex_normals));
}


#' @title Read file in FreeSurfer surface format or various mesh formats.
#'
#' @description Read a brain surface mesh consisting of vertex and face data from a file in FreeSurfer binary or ASCII surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'.
#'
#' @param filepath string. Full path to the input surface file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @param format one of 'auto', 'asc', 'vtk', 'ply', 'gii', 'mz3', 'stl', 'byu', 'geo', 'ico', 'tri', 'obj', 'off' or 'bin'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.asc'.
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

  if(!(format %in% c('auto', 'bin', 'asc', 'vtk', 'ply', 'gii', 'mz3', 'stl', 'byu', 'geo', 'ico', 'tri', 'obj', 'off'))) {
    stop("Format must be one of c('auto', 'bin', 'asc', 'vtk', 'ply', 'gii', 'mz3', 'stl', 'byu', 'geo', 'ico', 'tri', 'obj', 'off').");
  }

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(read.fs.surface.asc(filepath));
  }

  if(format == 'vtk' | (format == 'auto' & filepath.ends.with(filepath, c('.vtk')))) {
    return(read.fs.surface.vtk(filepath));
  }

  if(format == 'ply' | (format == 'auto' & filepath.ends.with(filepath, c('.ply')))) {
    return(read.fs.surface.ply(filepath));
  }

  if(format == 'gii' | (format == 'auto' & filepath.ends.with(filepath, c('.gii')))) {
    return(read.fs.surface.gii(filepath));
  }

  if(format == 'mz3' | (format == 'auto' & filepath.ends.with(filepath, c('.mz3')))) {
    return(read.fs.surface.mz3(filepath));
  }

  if(format == 'stl' | (format == 'auto' & filepath.ends.with(filepath, c('.stl')))) {
    return(read.fs.surface.stl(filepath));
  }

  if(format == 'byu' | (format == 'auto' & filepath.ends.with(filepath, c('.byu')))) {
    return(read.fs.surface.byu(filepath));
  }

  if(format == 'geo' | (format == 'auto' & filepath.ends.with(filepath, c('.geo')))) {
    return(read.fs.surface.geo(filepath));
  }

  if(format == 'obj' | (format == 'auto' & filepath.ends.with(filepath, c('.obj')))) {
    return(read.fs.surface.obj(filepath));
  }

  if(format == 'off' | (format == 'auto' & filepath.ends.with(filepath, c('.off')))) {
    return(read.fs.surface.off(filepath));
  }

  if(format == 'tri' | format == 'ico' | (format == 'auto' & filepath.ends.with(filepath, c('.tri'))) | (format == 'auto' & filepath.ends.with(filepath, c('.ico')))) {
    return(read.fs.surface.ico(filepath));
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

  #cur_pos = seek(fh, where=NA);
  #cat(sprintf("At position %d before reading anything.\n", cur_pos));

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
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));  # nocov
    }

    num_face_vertex_indices = num_faces * 3L;
    face_vertex_indices = readBin(fh, integer(), size = 4L, n = num_face_vertex_indices, endian = "big");   # a face is made of of 3 integers, which are vertex indices
    faces = matrix(face_vertex_indices, nrow=num_faces, ncol=3L, byrow = TRUE);
    faces = faces + 1L;    # Increment indices by 1: GNU R uses 1-based indices.

    if(length(face_vertex_indices) != num_face_vertex_indices) {
      stop(sprintf("Mismatch in read vertex indices for faces: expected %d but received %d.\n", num_face_vertex_indices, length(face_vertex_indices)));  # nocov
    }

  } else {
    stop(sprintf("Magic number mismatch (%d != (%d || %d)). The given file '%s' is not a valid FreeSurfer surface format file in binary format. (Hint: This function is designed to read files like 'lh.white' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, TRIS_MAGIC_FILE_TYPE_NUMBER, NEW_QUAD_MAGIC_FILE_TYPE_NUMBER, filepath));  # nocov
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
print.fs.surface <- function(x, ...) { # nocov start
  cat(sprintf("Brain surface trimesh with %d vertices and %d faces.\n", nrow(x$vertices), nrow(x$faces)));
  cat(sprintf("-Surface coordinates: minimal values are (%.2f, %.2f, %.2f), maximal values are (%.2f, %.2f, %.2f).\n", min(x$vertices[,1]), min(x$vertices[,2]), min(x$vertices[,3]), max(x$vertices[,1]), max(x$vertices[,2]), max(x$vertices[,3])));
  if(ncol(x$vertices) != 3L) {
    warning(sprintf("Vertex coordinates of the mesh have %d dimensions, expected 3. Not a valid 3-dimensional mesh.\n", ncol(x$vertices)));
  }
  if(ncol(x$faces) != 3L) {
    warning(sprintf("Faces of the mesh consist of %d vertices each, expected 3. Not a valid triangular mesh.\n", ncol(x$faces)));
  }
} # nocov end


#' @title Convert quadrangular faces or polygons to triangular ones.
#'
#' @param quad_faces nx4 integer matrix, the indices of the vertices making up the *n* quad faces.
#'
#' @return *2nx3* integer matrix, the indices of the vertices making up the *2n* tris faces.
#'
#' @note This function does no fancy remeshing, it simply splits each quad into two triangles.
#'
#' @family mesh functions
#'
#' @export
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
#' @export
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


#' @title Read GIFTI format mesh as surface.
#'
#' @param filepath string. Full path to the input surface file in GIFTI format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#' @family gifti readers
#'
#' @export
read.fs.surface.gii <- function(filepath) {
  if (requireNamespace("gifti", quietly = TRUE)) {
    gii = gifti::read_gifti(filepath);
    ret_list = list("vertices" = gii$data$pointset, "faces" = matrix(as.integer(gii$data$triangle + 1L), ncol=3L), "mesh_face_type" = 'tris');
    class(ret_list) = c("fs.surface", class(ret_list));
    return(ret_list);
  } else {
    stop("Reading GIFTI format surface files requires the package 'gifti' to be installed.");   # nocov
  }
}


#' @title Read surface mesh in mz3 format, used by Surf-Ice.
#'
#' @description The mz3 format is a binary file format that can store a mesh (vertices and faces), and optionally per-vertex colors or scalars.
#'
#' @param filepath full path to surface mesh file in mz3 format.
#'
#' @return an `fs.surface` instance. If the mz3 file contained RGBA per-vertex colors or scalar per-vertex data, these are available in the 'metadata' property.
#'
#' @references See https://github.com/neurolabusc/surf-ice for details on the format.
#'
#' @export
read.fs.surface.mz3 <- function(filepath) {
  is_gzipped = FALSE;
  fh = file(filepath, "rb");
  magic = readBin(fh, integer(), size = 2, n = 1, endian = "little");
  if(magic != 23117L) {
    close(fh);
    fh = gzfile(filepath, "rb");
    magic = readBin(fh, integer(), size = 2, n = 1, endian = "little");
    if(magic != 23117L) {
      close(fh);
      stop("File not in mz3 format");
    }
    is_gzipped = TRUE;
  }
  attr = readBin(fh, integer(), size = 2, n = 1, endian = "little");
  num_faces = magic = readBin(fh, integer(), size = 4, n = 1, endian = "little");
  num_vertices = readBin(fh, integer(), size = 4, n = 1, endian = "little");
  num_skip = readBin(fh, integer(), size = 4, n = 1, endian = "little");

  # cat(sprintf("mz3: magic=%d attr=%d faces=%d vertices=%d skip=%d. is_gz=%d\n", magic, attr, num_faces, num_vertices, num_skip, as.integer(is_gzipped)));

  is_face = bitwAnd(attr, 1L) != 0L;
  is_vert = bitwAnd(attr, 2L) != 0L;
  is_rgba = bitwAnd(attr, 4L) != 0L;
  is_scalar = bitwAnd(attr, 8L) != 0L;

  # cat(sprintf("mz3: face=%d vert=%d rgba=%d scalar=%d.\n", as.integer(is_face), as.integer(is_vert), as.integer(is_rgba), as.integer(is_scalar)));

  if(attr > 15L) {
    stop("Unsupported mz3 file version.");    # nocov
  }

  if(num_vertices < 1L) {
    stop("Mesh must contain at least one vertex.");    # nocov
  }
  if(is_face) {
    if(num_faces < 1L) {
      stop("Must contain at least one face is faces is set.");    # nocov
    }
  }

  #header_bytes = 16L; # these have already been read above.

  if(num_skip > 0L) {
    if(is_gzipped) {   # Cannot seek in a gzip stream
      discarded = readBin(fh, integer(), n = num_skip, size = 1L);
      discarded = NULL;
    } else {
        seek(fh, where=num_skip, origin="current");
    }
  }

  if(is_face) {
    faces_vertex_indices = readBin(fh, integer(), size = 4, n = num_faces * 3L, endian = "little");
    faces = matrix(faces_vertex_indices, nrow=num_faces, ncol=3L, byrow = TRUE);
    faces = faces + 1L;
  }
  if(is_vert) {
    vertex_coords = readBin(fh, numeric(), size = 4, n = num_vertices * 3L, endian = "little");
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L, byrow = TRUE);
  }
  vertex_colors = NULL;
  if(is_rgba) {
    vertex_colors = readBin(fh, integer(), size = 4, n = num_vertices, endian = "little");
  }
  scalars = NULL;
  if(is_scalar) {
    scalars = readBin(fh, numeric(), size = 4, n = num_vertices, endian = "little");
  }

  ret_list = list();
  ret_list$vertices = vertices;
  ret_list$faces = faces;
  ret_list$metadata = list("vertex_colors"=vertex_colors, "scalars"=scalars);
  class(ret_list) = c("fs.surface", class(ret_list));

  close(fh);
  return(ret_list);
}


#' @title Read surface mesh in STL binary format.
#'
#' @description The STL format is a mesh format that is often used for 3D printing, it stores geometry information. It is known as stereolithography format. A binary and an ASCII version exist. This function reads the binary version.
#'
#' @inheritParams read.fs.surface.stl.ascii
#'
#' @return an `fs.surface` instance.
#'
#' @references See https://en.wikipedia.org/wiki/STL_(file_format) for the format spec.
#'
#' @note The STL format does not use indices into a vertex list to define faces, instead it repeats vertex coords in each face ('polygon soup').
#'
#' @export
read.fs.surface.stl.bin <- function(filepath, digits = 6L) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  # skip header
  discarded = readBin(fh, integer(), n = 80L, size = 1L);
  discarded = NULL;

  num_faces = readBin(fh, integer(), size = 4, n = 1, endian = "little");

  # cat(sprintf("Reading %d faces from binary STL file.\n", num_faces));

  all_normals = NULL;
  all_vertex_coords = NULL;
  all_attr_counts = NULL;

  for(face_idx in seq.int(num_faces)) {
    face_normal = readBin(fh, double(), size = 4, n = 3L, endian = "little");
    vertex_coords = readBin(fh, double(), size = 4, n = 9L, endian = "little");
    vertex_coords = matrix(vertex_coords, nrow = 3, byrow = TRUE);
    attr_count = readBin(fh, integer(), size = 2, n = 1L, signed = FALSE, endian = "little");
    all_attr_counts = c(all_attr_counts, attr_count);

    if(is.null(all_normals)) {
      all_normals = face_normal;
    } else {
      all_normals = rbind(all_normals, face_normal);
    }
    if(is.null(all_vertex_coords)) {
      all_vertex_coords = vertex_coords;
    } else {
      all_vertex_coords = rbind(all_vertex_coords, vertex_coords);
    }
  }

  if(any(all_attr_counts != 0L)) {
    warning('Found non-zero face attribute count entries in file, ignored.');   # nocov
  }

  return(polygon.soup.to.indexed.mesh(all_vertex_coords, digits = digits));
}


#' @title Read mesh in STL format, auto-detecting ASCII versus binary format version.
#'
#' @inheritParams read.fs.surface.stl.ascii
#'
#' @param is_ascii logical, whether the file is in the ASCII version of the STL format (as opposed to the binary version). Can also be the character string 'auto', in which case the function will try to auto-detect the format.
#'
#' @return an `fs.surface` instance, the mesh.
#'
#' @note The mesh is stored in the file as a polygon soup, which is transformed into an index mesh by this function.
#'
#' @export
read.fs.surface.stl <- function(filepath, digits = 6L, is_ascii = 'auto') {

  if(is_ascii == 'auto') {
    is_ascii = stl.format.file.is.ascii(filepath);
  }

  if(is_ascii) {
    return(read.fs.surface.stl.ascii(filepath, digits = digits));
  } else {
    return(read.fs.surface.stl.bin(filepath, digits = digits));
  }
}


#' @title Guess whether a mesh file in STL format is the ASCII or the binary version.
#'
#' @inheritParams read.fs.surface.stl.bin
#'
#' @keywords internal
stl.format.file.is.ascii <- function(filepath) {
  stl_lines = NULL;
  stl_lines <- tryCatch({
    readLines(filepath);
  }, error = function(e) {
    NULL;
  }, warning = function(e) {
    NULL;
  });

  if(is.null(stl_lines)) {
    return(FALSE);
  }
  return(startsWith(stl_lines[1], "solid"));
}


#' @title Read surface mesh in STL ASCII format.
#'
#' @description The STL format is a mesh format that is often used for 3D printing, it stores geometry information. It is known as stereolithography format. A binary and an ASCII version exist. This function reads the ASCII version.
#'
#' @param filepath full path to surface mesh file in STL format.
#'
#' @param digits the precision (number of digits after decimal separator) to use when determining whether two x,y,z coords define the same vertex. This is used when the polygon soup is turned into an indexed mesh.
#'
#' @return an `fs.surface` instance. The normals are available in the 'metadata' property.
#'
#' @references See https://en.wikipedia.org/wiki/STL_(file_format) for a format description.
#'
#' @note The STL format does not use indices into a vertex list to define faces, instead it repeats vertex coords in each face ('polygon soup'). Therefore, the mesh needs to be reconstructed, which requires the `misc3d` package.
#'
#' @keywords internal
read.fs.surface.stl.ascii <- function(filepath, digits = 6L) {

  stl_lines = readLines(filepath);
  lines_total = length(stl_lines);
  if(lines_total < 8L) {
    stop("Invalid STL ASCII file: file must contain at least 8 lines (one face).");   # nocov
  }

  if(! startsWith(stl_lines[1], "solid")) {
    stop("Invalid STL ASCII file: first line must start with 'solid'.");   # nocov
  }

  line_idx = 1L;
  num_lines_left = lines_total - line_idx;

  all_normals = NULL;
  all_vertex_coords = NULL;

  while(num_lines_left >= 7L) {
    face_info = parse.stl.ascii.face(stl_lines[(line_idx+1L):(line_idx+7L)]);
    face_normal = face_info$face_normal;
    vertex_coords = face_info$vertex_coords;

    if(is.null(all_normals)) {
      all_normals = face_normal;
    } else {
      all_normals = rbind(all_normals, face_normal);
    }
    if(is.null(all_vertex_coords)) {
      all_vertex_coords = vertex_coords;
    } else {
      all_vertex_coords = rbind(all_vertex_coords, vertex_coords);
    }

    line_idx = line_idx + 7L;
    num_lines_left = lines_total - line_idx;
  }

  if(num_lines_left != 1L) {
    stop("Ignored %d lines at the end of ASCII STL file, please double-check file.\n", num_lines_left);   # nocov
  } else {
    if(! startsWith(stl_lines[lines_total], "endsolid")) {
      stop("Invalid STL ASCII file: last line must start with 'endsolid'.");   # nocov
    }
  }

  return(polygon.soup.to.indexed.mesh(all_vertex_coords, digits = digits));
}


#' @title Parse a single ASCII STL face.
#'
#' @param stl_face_lines vector of exactly 7 character strings, the lines from an STL ASCII file defining a triangular face.
#'
#' @return named list with entries: 'face_normal': double matrix with 1 row and 3 columns, the face normal. 'vertex_coords': double matrix with 3 rows and 3 columns, the 3x3 vertex coordinates of the face, each row contain the x, y, and z coordinate of a vertex.
#'
#' @keywords internal
parse.stl.ascii.face <- function(stl_face_lines) {
  if(length(stl_face_lines) != 7L) {
    stop(sprintf("Expected 7 STL lines, received %d.\n", length(stl_face_lines)));   # nocov
  }
  stl_face_lines = trimws(stl_face_lines); # trim leading and trailing white space from all lines.

  # the normal line is the fist of the 7 lines, and looks like this: 'facet normal 8.424343831663975e-20 6.016192594284479e-15 -1.0'
  face_normal = as.double(strsplit(stl_face_lines[1], " ")[[1]][3:5]);
  face_normal = matrix(face_normal, ncol = 3, nrow = 1);

  # the 3 vertex lines are lines 3 to 5 of the  lines, each one looks like this: 'vertex -19.848729961302116 -22.57002825204731 38.1169729018115'
  v1_coords = as.double(strsplit(stl_face_lines[3], " ")[[1]][2:4]);
  v2_coords = as.double(strsplit(stl_face_lines[4], " ")[[1]][2:4]);
  v3_coords = as.double(strsplit(stl_face_lines[5], " ")[[1]][2:4]);

  vertex_coords = rbind(v1_coords, v2_coords, v3_coords);
  colnames(vertex_coords) = c('x', 'y', 'z');
  rownames(vertex_coords) = NULL;

  return(list('face_normal'=face_normal, 'vertex_coords'=vertex_coords));
}


#' @title Turn polygon soup into indexed mesh.
#'
#' @description Some mesh file formats like STL do not store the faces as indices into a vertex list ('indexed mesh'), but repeat all vertex coordinates for each face ('polygon soup'). This function creates an indexed mesh from a polysoup.
#'
#' @param face_vertex_coords numerical matrix with *n* rows and 3 columns, the vertex coordinates of the faces. Each row contains the x,y,z coordinates of a single vertex, and three consecutive vertex rows form a triangular face.
#'
#' @param digits the precision (number of digits after decimal separator) to use when to determine whether two x,y,z coords define the same vertex.
#'
#' @return an indexed mesh, as an `fs.surface` instance (see \code{\link[freesurferformats]{read.fs.surface}}).
#'
#' @keywords internal
polygon.soup.to.indexed.mesh <- function(faces_vertex_coords, digits=6) {

  if(! is.matrix(faces_vertex_coords)) {
    stop("Parameter 'faces_vertex_coords' must be a matrix.");   # nocov
  }
  if(ncol(faces_vertex_coords) != 3L) {
    stop(sprintf("Parameter 'faces_vertex_coords' must be a matrix with exactly 3 columns, found %d.\n", ncol(faces_vertex_coords)));   # nocov
  }

  if((nrow(faces_vertex_coords) %% 3L) != 0L) {
    stop(sprintf("Parameter 'faces_vertex_coords' must be a matrix with row count a multiple of 3, but found %d rows.\n", nrow(faces_vertex_coords))); # nocov
  }
  #num_initial_faces = as.integer(nrow(faces_vertex_coords) / 3L);
  num_initial_vertices = nrow(faces_vertex_coords);

  # rounded version of coords for comparison.
  #faces_vertex_coords_rounded = round(faces_vertex_coords, digits = digits);
  coord_keys = apply(faces_vertex_coords, 1, coord.to.key, digits = digits);
  new_vertex_indices = rep(0L, num_initial_vertices);
  new_vertex_old_indices = rep(NA, num_initial_vertices); # reverse mapping. The length is unknown, but it cannot be more than the old ones.
  new_vertex_coords = NULL;

  unique_position_keys_to_index = list();
  current_unique_vertex_index = 1L;
  for(vert_idx_initial in seq.int(num_initial_vertices)) {
    vkey = coord_keys[vert_idx_initial];
    vcoords = faces_vertex_coords[vert_idx_initial,];
    if(is.null(unique_position_keys_to_index[[vkey]])) {
      unique_position_keys_to_index[[vkey]] = current_unique_vertex_index;
      new_vertex_indices[vert_idx_initial] = current_unique_vertex_index;

      # store coord for new vertex
      new_vertex_old_indices[current_unique_vertex_index] = vert_idx_initial;
      if(is.null(new_vertex_coords)) {
        new_vertex_coords = matrix(vcoords, ncol=3);
      } else {
        new_vertex_coords = rbind(new_vertex_coords, vcoords);
      }

      current_unique_vertex_index = current_unique_vertex_index + 1L;
    } else {
      new_vertex_indices[vert_idx_initial] = unique_position_keys_to_index[[vkey]];
    }
  }

  # build faces using vertex indices
  new_faces = matrix(new_vertex_indices, ncol = 3, byrow = TRUE);

  mesh = list('vertices'=new_vertex_coords, 'faces'=new_faces);
  class(mesh) = c(class(mesh), 'fs.surface');
  return(mesh);
}


#' @title Turn coordinate vector into string.
#'
#' @param coord double vector of length 3, the xyz coord
#'
#' @param digits integer, the number of digits (after the decimal separator) to use
#'
#' @return character string
#'
#' @keywords internal
coord.to.key <- function(coord, digits=6L) {
  format_string = sprintf("%%.%df,%%.%df,%%.%df", digits, digits, digits);
  return(sprintf(format_string, coord[1], coord[2], coord[3]));
}


#' @title Read mesh in BYU format.
#'
#' @description The BYU or Brigham Young University format is an old ASCII mesh format that is based on fixed character positions in lines (as opposed to whitespace-separated elements). I consider it a bit counter-intuitive.
#'
#' @param filepath full path of the file in BYU format.
#'
#' @param part positive integer, the index of the mesh that should be loaded from the file. Only relevant if the file contains more than one mesh.
#'
#' @return an `fs.surface` instance, aka a mesh
#'
#' @references See http://www.eg-models.de/formats/Format_Byu.html for a format description.
#'
#' @importFrom stats na.omit
#' @export
read.fs.surface.byu <- function(filepath, part = 1L) {
  part = as.integer(part);
  byu_lines = readLines(filepath);

  element_counts = as.integer(linesplit.fixed(byu_lines[1], length_per_part=6L, num_parts_expected=5L, error_tag = "1"));
  num_parts = element_counts[1]; # number of meshes in the file.
  num_vertices = element_counts[2];
  num_faces = element_counts[3]; # strictly these faces are polys, not neccessarily triangles. We only support triangles though.
  num_connects = element_counts[4];
  num_test = element_counts[5];

  if(num_test != 0L) {
    stop(sprintf("Not a valid BYU mesh file: num_test must be 0, but is '%d'.\n", num_test));
  }

  vertices_per_face = 3L; # assume triangular mesh
  if(num_connects != (3L * num_faces)) {
    vertices_per_face = num_connects / num_faces;
    if(vertices_per_face == 4L) {
      message(sprintf("Mesh has %d vertices per face (quadrangular faces), remeshing to triangular faces.\n", vertices_per_face));
    } else {
      stop(sprintf("Only triangular or quadrangular meshes in BYU files are supported: expected %d edges for %d triangular faces, but found %d. Mesh has %d vertices per face.\n", (3L * num_faces), num_faces, num_connects, vertices_per_face)); # nocov
    }
  }
  if(part > num_parts) {
    stop(sprintf("Requested to load mesh # %d from BYU file, but the file contains %d meshes only.\n", part, num_parts));
  }
  if(num_vertices < vertices_per_face | num_faces < 1L) {
    stop("Mesh file does not contain any faces.");   # nocov
  }
  #relevant_part_info_line_index = part + 1L;
  #part_info = as.integer(linesplit.fixed(byu_lines[relevant_part_info_line_index], length_per_part=6L, num_parts_expected=2L, error_tag = relevant_part_info_line_index));
  #part_start = part_info[1];  # the first vertex index (by one-based index in the face list) of this mesh
  #part_end = part_info[2];    # the last vertex index (by one-based index in the face list) of this mesh

  # Read the point lines. Each line contains the x, y, z coords for 2 vertices (=2 x 3 numbers), the last line may of
  # course only contain the coords for a single vertex.
  all_coords = NULL;
  first_vertex_coords_line_index = 1L + num_parts + 1L;
  num_verts_left_to_parse = num_vertices;
  current_line_idx = first_vertex_coords_line_index;
  chars_per_coord = 12L;
  while(num_verts_left_to_parse > 0L) {
    cline = byu_lines[current_line_idx];
    coords = as.double(linesplit.fixed(cline, length_per_part=chars_per_coord, num_parts_expected=NULL, error_tag = current_line_idx));
    coords = stats::na.omit(coords); # If a line (the last one) is not fully filled with digits, the missing ones will be parsed as NA. Remove these NAs.
    if(length(coords) == 6L) {
      num_verts_left_to_parse = num_verts_left_to_parse - 2L;
    } else if(length(coords) == 3L) {
      num_verts_left_to_parse = num_verts_left_to_parse - 1L;
    } else {
      stop(sprintf("Expected 3 or 6 vertex coordinates per BYU file line, but found %d in line # %d. Mesh not 3-dimensional?\n", length(coords), current_line_idx)); # nocov
    }
    coords = matrix(coords, ncol = 3L, byrow = TRUE);
    if(is.null(all_coords)) {
      all_coords = coords;
    } else {
      all_coords = rbind(all_coords, coords);
    }
    current_line_idx = current_line_idx + 1L;
  }

  if(nrow(all_coords) != num_vertices) {
    stop(sprintf("BYU data mismatch: expected %d vertices from header, but found %d.\n", num_vertices, nrow(all_coords)));   # nocov
  }

  # Parse faces. For now, we only parse the vertex indices. We construct faces from them later.
  all_faces_vert_indices = NULL; # only a vector for now, not a matrix.
  num_faces_vert_indices_left_to_parse = num_faces * vertices_per_face;
  chars_per_vertex_index = 6L;
  while(num_faces_vert_indices_left_to_parse > 0L) {
    cline = byu_lines[current_line_idx];
    faces_vert_indices = as.integer(linesplit.fixed(cline, length_per_part=chars_per_vertex_index, num_parts_expected=NULL, error_tag = current_line_idx));
    faces_vert_indices = stats::na.omit(faces_vert_indices);

    num_faces_vert_indices_left_to_parse = num_faces_vert_indices_left_to_parse - length(faces_vert_indices);
    if(is.null(all_faces_vert_indices)) {
      all_faces_vert_indices = faces_vert_indices;
    } else {
      all_faces_vert_indices = c(all_faces_vert_indices, faces_vert_indices);
    }
    current_line_idx = current_line_idx + 1L;
  }

  # Now create faces from the vector of vertex indices. If an index is negative, it is the last one in the current face.
  last_vertex_of_face_indices = which(all_faces_vert_indices < 0L);
  if(length(last_vertex_of_face_indices) != num_faces) {
    stop(sprintf("BYU data mismatch: expected %d face end vertices from header, but found %d.\n", num_faces, length(last_vertex_of_face_indices)));   # nocov
  }

  all_faces_vert_indices = as.integer(abs(all_faces_vert_indices));
  faces = matrix(all_faces_vert_indices, ncol = vertices_per_face, byrow = TRUE);

  mesh = list('vertices'=all_coords, 'faces'=faces);

  # Remesh quadrangular mesh to triangular if needed.
  if(vertices_per_face == 4L) {
    mesh$metadata = list('faces_quads'=faces);
    mesh$faces = faces.quad.to.tris(mesh$faces);
  }

  class(mesh) = c(class(mesh), 'fs.surface');
  return(mesh);
}


#' @title Split a string into fixed-length parts.
#'
#' @param cline character string, the input line
#'
#' @param length_per_part integer, number of characters per part
#'
#' @param num_parts_expected integer, the number of parts. Leave at NULL if this is not known.
#'
#' @param error_tag optional character string, how to identify the line in a parsing error message. Could be the line number, or whatever. Only relevant if 'num_parts_expected' is not matched.
#'
#' @keywords internal
linesplit.fixed <- function(cline, length_per_part, num_parts_expected=NULL, error_tag=NULL) {

  if(is.null(error_tag)) {
    error_tag_string = "";
  } else {
    error_tag_string = sprintf('%s ', as.character(error_tag));
  }

  if((nchar(cline) %% length_per_part) != 0L) {
    stop(sprintf("Line %slength %d is not a multiple of the length per part (%d).\n", error_tag_string, nchar(cline), length_per_part));
  }

  if(! is.null(num_parts_expected)) {
    num_chars_expected = num_parts_expected * length_per_part;
    if(nchar(cline) != num_chars_expected) {
      stop(sprintf("Line %slength of %d characters expected for %d parts with length %d, but found line with %d chars.\n", error_tag_string, num_chars_expected, num_parts_expected, length_per_part, nchar(cline)));
    }
  }
  start_indices = seq(1L, nchar(cline)-1L, by=length_per_part);
  stop_indices = seq(length_per_part, nchar(cline), by=length_per_part);

  if(length(start_indices) != length(stop_indices)) {
    stop(sprintf("Line %slength %d is not a multiple of the length per part (%d) -- part index mistmatch.\n", error_tag_string, nchar(cline), length_per_part));
  }

  line_parts = substring(cline, start_indices, stop_indices);
  if(! is.null(num_parts_expected)) {
    if(length(line_parts) != num_parts_expected) {
      stop(sprintf("Parsed line %scontains %d fields, expected %d.", error_tag_string, length(line_parts), num_parts_expected));
    }
  }
  return(line_parts);
}


#' @title Read ICO format mesh as surface.
#'
#' @description This reads meshes from text files in ICO / TRI mesh format. This format is not to be confused with the the image format used to store tiny icons.
#'
#' @param filepath string. Full path to the input surface file in ICO or TRI mesh format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is a fixed width format.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.ico <- function(filepath) {

  ico_lines = readLines(filepath);
  num_verts = as.integer(trimws(ico_lines[1]));   # "%8d"

  vertices_df = NULL;
  faces_df = NULL;

  # This is a fixed with format, but there is always a space between the separate fields, so we can read it with read.table instead of splitting strings.

  current_line_idx = 1L;

  # vertex line: "%8d %8.4f %8.4f %8.4f\n"
  vertices_df = read.table(filepath, skip=current_line_idx, col.names = c('vertex_index', 'coord1', 'coord2', 'coord3'), colClasses = c("integer", "numeric", "numeric", "numeric"), nrows=num_verts, header=FALSE);
  current_line_idx = current_line_idx + num_verts + 1L;
  num_faces = as.integer(trimws(ico_lines[current_line_idx]));    # "%8d"

  # face line: vertex indices are 1 based, and the vertex ordering is flipped to clockwise!
  # fprintf(fp, "%8d %8d %8d %8d\n", actual_fno, face->v[0] + 1, face->v[2] + 1, face->v[1] + 1);
  faces_df = read.table(filepath, skip=current_line_idx, col.names = c('face_index', 'vert1', 'vert3', 'vert2'), colClasses = c("integer", "integer", "integer", "integer"), nrows=num_faces, header=FALSE);

  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[2:4]));
  ret_list$faces = unname(data.matrix(faces_df[2:4]));  # we do not add +1 here: in ICO files the indices are already 1-based
  # switch vertex order
  tmp_vec = ret_list$faces[,2];
  ret_list$faces[,2] = ret_list$faces[,3];
  ret_list$faces[,3] = tmp_vec;
  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ICO mesh file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));   # nocov
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in ICO mesh file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));    # nocov
  }

  return(ret_list);
}


#' @title Read GEO format mesh as surface.
#'
#' @description This reads meshes from text files in GEO mesh format. This is an ASCII format.
#'
#' @param filepath string. Full path to the input surface file in GEO mesh format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is a fixed width format.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.geo <- function(filepath) {

  header_line1_data = read.table(filepath, colClasses = rep('integer', 4L), col.names = c('one', 'num_verts', 'num_faces', 'num_connect'), nrows = 1L);
  num_verts = header_line1_data$num_verts;
  num_faces = header_line1_data$num_faces;

  if(header_line1_data$num_connect != num_faces * 3L) {
    stop("Inconsistent GEO header data or not a triangular mesh.");
  }

  #cat(sprintf("Reading %d vertices and %d faces from GEO file.\n", num_verts, num_faces));

  # we skip the 2nd header line entirely, it is redundant (contains number of faces once more).
  current_line_idx = 2L;

  num_full_vertex_lines = num_verts / 2L;
  vertices_two_per_line_df = read.table(filepath, skip = current_line_idx, colClasses = rep('double', 6L), col.names = c('v1_x', 'v1_y', 'v1_z', 'v2_x', 'v2_y', 'v2_z'), nrows = num_full_vertex_lines);
  # Now transform to a single 3-col matrix:
  left_vertices = unname(data.matrix(vertices_two_per_line_df[1:3]));
  right_vertices = unname(data.matrix(vertices_two_per_line_df[4:6]));
  vertices = as.double((t(cbind(left_vertices, right_vertices))));
  vertices = matrix(vertices, ncol = 3, byrow = TRUE);

  current_line_idx = current_line_idx + num_full_vertex_lines;
  if(num_verts %% 2L == 1L) {
    # We still need to add the single vertex on the last line.
    last_vert = read.table(filepath, skip = current_line_idx, colClasses = rep('double', 3L), col.names = c('v1_x', 'v1_y', 'v1_z'), nrows = 1L);
    vertices = rbind(vertices, unname(data.matrix(last_vert[1:3])));
    current_line_idx = current_line_idx + 1L;
  }

  # face lines: vertex indices are 1 based.
  faces_df = read.table(filepath, skip=current_line_idx, col.names = c('vert1', 'vert2', 'vert3'), colClasses = c("integer", "integer", "integer"), nrows=num_faces);
  faces_df$vert3 = abs(faces_df$vert3);  # last index in every poly is negative to mark end of poly

  ret_list = list();
  ret_list$vertices = vertices;
  ret_list$faces = unname(data.matrix(faces_df[1:3]));  # we do not add +1 here: in ICO GEO the indices are already 1-based

  # switch vertex order in faces
  tmp_vec = ret_list$faces[,2];
  ret_list$faces[,2] = ret_list$faces[,3];
  ret_list$faces[,3] = tmp_vec;

  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in GEO mesh file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));  # nocov
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in GEO mesh file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));   # nocov
  }

  return(ret_list);
}


#' @title Read OBJ format mesh as surface.
#'
#' @description This reads meshes from text files in Wavefront OBJ mesh format. This is an ASCII format.
#'
#' @param filepath string. Full path to the input surface file in Wavefront object mesh format. Files with non-standard vertex colors (3 additional float fields after the vertex coordinates in order R, G, B) are supported, and the colors will be returned in the field 'vertex_colors' if present.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is a simple but very common mesh format supported by many applications, well suited for export.
#'
#' @family mesh functions
#' @export
read.fs.surface.obj <- function(filepath) {

  verts_and_faces_df = NULL;
  uses_vertex_color = FALSE;
  verts_and_faces_df = tryCatch({
    read.table(filepath, colClasses = c('character', 'numeric', 'numeric', 'numeric'), col.names = c('type', 'val1', 'val2', 'val3'));
  }, error = function(e) {
    NULL;
  });

  if(is.null(verts_and_faces_df)) {
    uses_vertex_color = TRUE;
    verts_and_faces_df = read.table(filepath, colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'), col.names = c('type', 'val1', 'val2', 'val3', 'col_r', 'col_b', 'col_g'), fill = TRUE);
  }

  first_face_row_idx = min(which(verts_and_faces_df$type == 'f'));

  num_vertices = first_face_row_idx - 1L;
  num_faces = nrow(verts_and_faces_df) - num_vertices;

  ret_list = list();

  if(uses_vertex_color) {
    vertices_df = read.table(filepath, colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'), col.names = c('type_vertex', 'coordx', 'coordy', 'coordz', 'col_r', 'col_b', 'col_g'), nrows = num_vertices);
    ret_list$vertex_colors = unname(data.matrix(vertices_df[5:7]));
  } else {
    vertices_df = read.table(filepath, colClasses = c('character', 'numeric', 'numeric', 'numeric'), col.names = c('type_vertex', 'coordx', 'coordy', 'coordz'), nrows = num_vertices);
  }
  faces_df = read.table(filepath, skip=num_vertices, colClasses = c('character', 'integer', 'integer', 'integer'), col.names = c('type_face', 'v1', 'v2', 'v3'), nrows = num_faces);


  ret_list$vertices = unname(data.matrix(vertices_df[2:4]));
  ret_list$faces = unname(data.matrix(faces_df[2:4]));
  class(ret_list) = c("fs.surface", class(ret_list));
  return(ret_list);
}


#' @title Read Object File Format (OFF) mesh as surface.
#'
#' @description This reads meshes from text files in OFF mesh format. This is an ASCII format.
#'
#' @param filepath string. Full path to the input surface file in OFF mesh format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#' @export
read.fs.surface.off <- function(filepath) {
  off_header = readLines(filepath, n = 1L);
  if(off_header != "# OFF") {
    stop("Not a valid Object Format File (OFF): missing expected header line.");
  }
  element_count_df = read.table(filepath, skip=1L, colClasses = c('integer', 'integer', 'integer'), col.names = c('num_vertices', 'num_faces', 'num_edges'), nrows = 1L);

  num_verts = element_count_df$num_vertices;
  num_faces = element_count_df$num_faces;

  vertices_df = read.table(filepath, skip = 2L, colClasses = c('numeric', 'numeric', 'numeric'), col.names = c('coordx', 'coordy', 'coordz'), nrows = num_verts);
  faces_df = read.table(filepath, skip=num_verts + 2L, colClasses = c('integer', 'integer', 'integer', 'integer'), col.names = c('num_verts', 'v1', 'v2', 'v3'), nrows = num_faces);

  ret_list = list();
  ret_list$vertices = unname(data.matrix(vertices_df[1:3]));
  ret_list$faces = unname(data.matrix(faces_df[2:4]));
  ret_list$faces = adjust.face.indices.to(ret_list$faces, target_min_index = 1L);
  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in OFF mesh file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));   # nocov
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in OFF mesh file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));    # nocov
  }

  return(ret_list);
}


#' @title Adjust integer matrix to target min value.
#'
#' @description This takes a matrix of integers, and adjusts the values such that the minimal value is the 'target_min_index' value. It is used to adjust from 0-based to 1-based indices in meshes.
#'
#' @param faces 3xn integer matrix, the vertex indices of the faces
#'
#' @param target_min_index integer, one of 1L or 0L. The target minimal value that the data should have afterwards.
#'
#' @return 3xn integer matrix, the adjusted values
#'
#' @note The current and the target min values must be 0 or 1.
#'
#' @keywords internal
adjust.face.indices.to <- function(faces, target_min_index=1L) {
  if(! is.matrix(faces)) {
    stop("Parameter 'faces' must be a matrix.");
  }
  target_min_index = as.integer(target_min_index);
  if(! target_min_index %in% c(0L, 1L)) {
    stop("Parameter 'target_min_index' must be 0 or 1.");
  }
  min_index = min(faces);
  if(! min_index %in% c(0L, 1L)) {
    stop(sprintf("Found current minimal vertex index %d, expected 0 or 1.\n", min_index));
  }
  if(min_index != target_min_index) {
    if(target_min_index == 1L) {
      faces = faces + 1L;
    } else {
      faces = faces - 1L;
    }
  }
  return(faces);
}



#' @title Read Brainvoyager srf format (.srf) mesh as surface.
#'
#' @description Read a mesh and associated data like color and normals from a binary file in BrainVoyager SRF mesh format.
#'
#' @param filepath string. Full path to the input surface file in SRF mesh format.
#'
#' @return fs.surface instance
#'
#' @references The srf format spec is at https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/344-users-guide-2-3-the-format-of-srf-files.
#'
#' @family mesh functions
#' @export
read.fs.surface.bvsrf <- function(filepath) {
  srf_mesh = read.mesh.brainvoyager(filepath);
  ret_list = list();
  ret_list$vertices = srf_mesh$vertices;
  ret_list$faces = srf_mesh$faces + 1L;
  class(ret_list) = c("fs.surface", class(ret_list));
  return(ret_list);
}


#' @title Read Brainvoyager srf format (.srf) mesh.
#'
#' @description Read a mesh and associated data like color and normals from a binary file in BrainVoyager SRF mesh format.
#'
#' @param filepath string. Full path to the input surface file in SRF mesh format.
#'
#' @return named list of the elements in the file.
#'
#' @references The srf format spec is at https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/344-users-guide-2-3-the-format-of-srf-files.
#'
#' @family mesh functions
#' @export
#' @importFrom grDevices rgb
read.mesh.brainvoyager <- function(filepath) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$srf_version = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
  ret_list$reserved = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  num_verts = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  num_faces = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$mesh_center_xyz = readBin(fh, numeric(), size = 4, n = 3, endian = endian);
  vert_coords_x = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian);
  vert_coords_y = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian);
  vert_coords_z = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian);
  vert_normals_x = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian); # see spec for gotcha, these point inwards!
  vert_normals_y = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian);
  vert_normals_z = readBin(fh, numeric(), size = 4, n = num_verts, endian = endian);
  ret_list$color_rgba_curv_convex = readBin(fh, numeric(), size = 4, n = 4, endian = endian);    # color for convex vertices, to add a binary color overlay based on curvature (float in range 0-1).
  ret_list$color_rgba_curv_concave = readBin(fh, numeric(), size = 4, n = 4, endian = endian);   # in range 0-1.

  # This is a raw color index, see the spec for the interpretation. Depending on the value,
  # it may reference (1) once of the 2 curvature colors above (2) an index into a color lookup table stored
  # below, or (3) a custom RGBA color.
  ret_list$vertex_colors_raw_index = readBin(fh, integer(), size = 4, n = num_verts, endian = endian);

  # Read vertex neighborhood info.
  current_center_vertex = 1L;
  while(current_center_vertex <= num_verts) {
    num_neighbors = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    neighbor_verts = readBin(fh, integer(), size = 4, n = num_neighbors, endian = endian);
    neighbor_verts = NULL; # We have to read the neighbor data, but we currently do not save it.
    current_center_vertex = current_center_vertex + 1L;
  }

  faces_vert_indices = readBin(fh, integer(), size = 4, n = num_faces * 3L, endian = endian);
  ret_list$faces = matrix(faces_vert_indices, ncol = 3, byrow = TRUE);
  num_triangle_strips = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  if(num_triangle_strips > 0L) { # triangle strips for faster rendering
    ret_list$triangle_strips = readBin(fh, integer(), size = 4, n = num_triangle_strips, endian = endian);
  }
  ret_list$mtc_file_name = readBin(fh, character(), n = 1);
  # Here follows a zero-terminated MTC file name, which we currently ignore.
  # End of parsing code.

  # We could derive the actual colors for the vertices here from the color table, curv colors, and direct colors and store them in ret_list$derived or similar.
  derived_colors = rep(NA, num_verts);
  derived_colors[ret_list$vertex_colors_raw_index == 0L] = grDevices::rgb(ret_list$color_rgba_curv_convex[1], ret_list$color_rgba_curv_convex[2], ret_list$color_rgba_curv_convex[3]);
  derived_colors[ret_list$vertex_colors_raw_index == 1L] = grDevices::rgb(ret_list$color_rgba_curv_concave[1], ret_list$color_rgba_curv_concave[2], ret_list$color_rgba_curv_concave[3]);
  ret_list$derived = list('curv_colors' = derived_colors);

  #num_curvcol = length(which(ret_list$vertex_colors_raw_index == 0L)) + length(which(ret_list$vertex_colors_raw_index == 1L));
  #cat(sprintf("%d verts total, found %d curv colors, %d color codes.\n", num_verts, num_curvcol, length(which(ret_list$vertex_colors_raw_index >= 1056964608L))));

  ret_list$vertices = cbind(vert_coords_x, vert_coords_y, vert_coords_z);
  ret_list$vertex_normals = cbind(vert_normals_x, vert_normals_y, vert_normals_z);
  return(ret_list);
}


#' @title Convert 32 bit integer to RGB color as described in Brainvoyager SRF file spec.
#'
#' @param int_val the 32 bit integer
#'
#' @return an rgb color
#'
#' @keywords internal
#' @importFrom grDevices rgb
int.to.col.brainvoyager <- function(int_val) {
  if(int_val < 1056965353L) {
    warning("Not a valid Brainvoyager color code, int_val must be >= 1056965353L.");
  }
  bits_32 = intToBits(int_val);
  raw_zeros_32 = intToBits(0L);
  red_bits = raw_zeros_32;
  blue_bits = raw_zeros_32;
  green_bits = raw_zeros_32;
  unused_bits = raw_zeros_32;
  red_bits[1:8] = bits_32[1:8];
  green_bits[1:8] = bits_32[9:16];
  blue_bits[1:8] = bits_32[17:24];
  unused_bits[1:8] = bits_32[25:32];
  r = packBits(red_bits, "integer");
  g = packBits(green_bits, "integer");
  b = packBits(blue_bits, "integer");
  #u = packBits(unused_bits, "integer");
  #cat(sprintf("%d => %d, %d, %d (%d)\n", int_val, r, g, b, u));
  return(grDevices::rgb(r/255., g/255., b/255.));
}


#' @title Stop unless surf is an fs.surface
#'
#' @param surf fs.surface instance or anything else
#'
#' @param param_name character string, used in stop message to identify the parameter.
#'
#' @return Called for the side effect of stopping if surf is not an fs.surface instance.
#'
#' @keywords internal
assert.surface <- function(surface, param_name="surface") {
  if(! is.fs.surface(surface)) {
    stop(sprintf("Parameter '%s' must be an fs.surface instance.", param_name));
  }
  return(invisible(NULL));
}
