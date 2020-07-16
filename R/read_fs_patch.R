#' @title Read FreeSurfer binary or ASCII patch file.
#'
#' @description A patch is a subset of a surface. Note that the contents of ASCII and binary patch format files is different. A binary format patch contains vertices only, without connection (face) information. ASCII patch files can also contain face data. See the return value description for details.
#'
#' @param filepath string. Full path to the input patch file. An example file is `FREESURFER_HOME/subjects/fsaverage/surf/lh.cortex.patch.3d`.
#'
#' @param format one of 'auto', 'asc', or 'bin'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.asc'.
#'
#' @return named list with 2 entries: "faces": can be NULL, only available if the format is ASCII, see return value of \code{\link[freesurferformats]{read.fs.patch.asc}}. "vertices": numerical *n*x7 matrix. The columns are named, and appear in the following order: 'vert_index1': the one-based (R-style) vertex index. 'x', 'y', 'z': float vertex coordinates. 'is_border': integer, 1 if the vertex lies on the patch border, 0 otherwise (treat as logical). 'raw_vtx': integer, the raw vtx value encoding index and border. 'vert_index0': the zero-based (C-style) vertex index.
#'
#' @family patch functions
#'
#' @export
read.fs.patch <- function(filepath, format='auto') {

  if(!(format %in% c('auto', 'bin', 'asc'))) {
    stop("Format must be one of c('auto', 'bin', 'asc').");
  }

  if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
    return(read.fs.patch.asc(filepath));
  }

  if(guess.filename.is.gzipped(filepath)) {
    fh = gzfile(filepath, "rb");
  } else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();

  version = readBin(fh, integer(), size = 4,endian = "big");

  if(version != -1L) {
    stop(sprintf("Binary patch file version is '%d', but expected version '-1'. File version not supported.\n", version));
  }

  num_points = readBin(fh, integer(), size = 4, endian = "big");  # the 'points' are actually vertex coordinates

  #message(sprintf("About to read %d points from version %d binary patch file.\n", num_points, version));

  points = matrix(rep(0., num_points*7), ncol=7);
  colnames(points) = c("vert_index1", "x", "y", "z", "is_border", "vtx_raw", "vert_index0");
  for(idx in seq_len(num_points)) {
    vtx = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    points[idx, 1] = abs(vtx);
    points[idx, 2:4] = readBin(fh, numeric(), size = 4, n = 3, endian = "big");
    points[idx, 5] = ifelse(vtx < 0, 1, 0);
    points[idx, 6] = vtx;
  }
  points[, 7] = points[, 1] - 1L;

  ret_list$vertices = points;
  ret_list$faces = NULL;    # binary patch file contains no face information

  class(ret_list) = c("fs.patch", class(ret_list));
  return(ret_list);
}



## To get a patch file in ASCII format, use 'mris_convert -p' on a binary patch file like this on the command line:
## mris_convert -p $FREESURFER_HOME/subjects/fsaverage/surf/lh.cortex.patch.3d ~/patch.asc


#' @title Read FreeSurfer ASCII format patch.
#'
#' @description An ASCII format patch is a part of a brain surface mesh, and is a mesh itself. It consists of vertices and faces. The ASCII patch format is very similar to the ASCII surface format. **Note:** The contents of ASCII and binary patch format files is different. The ASCII patch format is not ideal for parsing, and loading such files is currently quite slow.
#'
#' @param filepath string. Full path to the input patch file in ASCII patch format.
#'
#' @return named list. The list has the following named entries: "vertices": see return value of \code{\link[freesurferformats]{read.fs.patch}}. "faces": numerical *n*x5 matrix. The columns are named, and appear in the following order: 'face_index1': the one-based (R-style) face index. 'vert1_index1', 'vert2_index1', 'vert3_index1': integer vertex indices of the face, they are one-based (R-style). 'face_index0': the zero-based (C-style) face index.
#'
#' @family patch functions
#'
#' @export
read.fs.patch.asc <- function(filepath) {

  num_verts_and_faces_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts', 'num_faces'), colClasses = c("integer", "integer"));
  num_verts = num_verts_and_faces_df$num_verts[1];
  num_faces = num_verts_and_faces_df$num_faces[1];


  fh = file(filepath, "r");
  patch_lines = readLines(fh);
  num_lines = length(patch_lines);
  on.exit({ close(fh) }, add=TRUE);

  #message(sprintf("Read %d lines from ASCII patch file. Info on %d verts, %d faces.\n", num_lines, num_verts, num_faces));

  expected_line_count = 2L + num_verts * 2L + num_faces * 2L;    # the first 2 are the header lines.

  if(expected_line_count != num_lines) {
    stop(sprintf("Expected %d lines in ASCII patch file, but received %d.\n", expected_line_count, num_lines));
  }

  vert_lines_start = 3L;
  vert_lines_end = vert_lines_start + (num_verts * 2L) - 1L;
  vertex_info_lines = patch_lines[vert_lines_start:vert_lines_end];

  face_lines_start = vert_lines_end + 1;
  face_lines_end = face_lines_start + (num_faces * 2L) - 1L;
  face_info_lines = patch_lines[face_lines_start:face_lines_end];

  #message(sprintf("Found %d vertex info lines for %d vertices. Found %d face info lines for %d faces.\n", length(vertex_info_lines), num_verts, length(face_info_lines), num_faces));

  ret_list = list();

  vertices = matrix(rep(0., num_verts*7), ncol=7);
  colnames(vertices) = c("vert_index1", "x", "y", "z", "is_border", "vtx_raw", "vert_index0");

  current_vert = 1L;
  for(vert_line_idx in seq_len(length(vertex_info_lines))) {
    vertex_line = vertex_info_lines[vert_line_idx];
    if(vert_line_idx %% 2 == 1L) {
      # uneven line, looks like this: '10 vno=9'. This is the vtx_raw, encoding vertex number and is_border property.
      line_parts = strsplit(vertex_line, "\\s+");
      vtx_raw = as.integer(unlist(line_parts)[1]);
      vertices[current_vert, 6] = vtx_raw;                    # vtx_raw
      vertices[current_vert, 5] = ifelse(vtx_raw < 0, 1, 0);  # is_border
      vertices[current_vert, 1] = abs(vtx_raw);               # vert_index1
      vertices[current_vert, 7] = abs(vtx_raw) - 1L;          # vert_index0
    } else {
      # even line, looks like this: '-5.624130  56.519470  -36.471378'. These are the coords.
      coords = as.double(unlist(strsplit(vertex_line, "\\s+")));
      vertices[current_vert, 2:4] = coords;
      current_vert = current_vert + 1L;
    }
  }

  ret_list$vertices = vertices;


  faces = matrix(rep(0L, num_faces*5), ncol=5);
  colnames(faces) = c("face_index1", "vert1_index1", "vert2_index1", "vert3_index1", "face_index0");

  current_face = 1L;
  for(face_line_idx in seq_len(length(face_info_lines))) {
    face_line = face_info_lines[face_line_idx];
    if(face_line_idx %% 2 == 1L) {
      # uneven line, looks like this: '106621'. This is the vtx_raw, encoding vertex number and is_border property.
      face_index0 = as.integer(face_line);
      faces[current_face, 5] = face_index0;
      faces[current_face, 1] = face_index0 + 1;
    } else {
      # even line, looks like this: '61548 61549 12836'. These are the 0-based vertex indices of the face.
      face_vertex_indices0 = as.integer(unlist(strsplit(face_line, "\\s+")));
      faces[current_face, 2:4] = face_vertex_indices0 + 1L;
      current_face = current_face + 1L;
    }
  }

  if(nrow(vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ASCII patch file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));
  }

  if(nrow(faces) != num_faces) {
    stop(sprintf("Expected %d faces in ASCII patch file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));
  }

  ret_list$faces = faces;
  class(ret_list) = c("fs.patch", class(ret_list));

  return(ret_list);
}
