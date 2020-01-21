#' @title Read FreeSurfer binary patch file.
#'
#' @description A binary format patch contains vertices only, without connection (face) information. **Note:** The contents of ASCII and binary patch format files is different.
#'
#' @param filepath string. Full path to the input patch file. An example file is `FREESURFER_HOME/subjects/fsaverage/surf/lh.cortex.patch.3d`.
#'
#' @return named list with entry: "data": numerical *n*x7 matrix. The columns are named, and appear in the following order: 'vert_index1': the one-based (R-style) vertex index. 'x', 'y', 'z': float vertex coordinates. 'is_border': integer, 1 if the vertex lies on the patch border, 0 otherwise (treat as logical). 'raw_vtx': integer, the raw vtx value encoding index and border. 'vert_index0': the zero-based (C-style) vertex index.
#'
#' @family patch functions
#'
#' @export
read.fs.patch <- function(filepath) {

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

  message(sprintf("About to read %d points from version %d binary patch file.\n", num_points, version));

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

  ret_list$data = points;

  return(ret_list);
}


#' @title Read FreeSurfer ASCII format patch.
#'
#' @description An ASCII format patch is a part of a brain surface mesh, and is a mesh itself. It consists of vertices and faces. The ASCII patch format is very similar to the ASCII surface format. **Note:** The contents of ASCII and binary patch format files is different.
#'
#' @param filepath string. Full path to the input patch file in ASCII surface format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you may need to adjust the index to compare with data from other software that uses zero-based indexing.
#'
#' @family patch functions
#'
#' @export
read.fs.patch.asc <- function(filepath) {

  num_verts_and_faces_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts', 'num_faces'), colClasses = c("integer", "integer"));
  num_verts = num_verts_and_faces_df$num_verts[1];
  num_faces = num_verts_and_faces_df$num_faces[1];

  vertices_df = read.table(filepath, skip=2L, col.names = c('coord1', 'coord2', 'coord3'), colClasses = c("numeric", "numeric", "numeric"), nrows=num_verts);

  faces_df = read.table(filepath, skip=2L + num_verts, col.names = c('vertex1', 'vertex2', 'vertex3'), colClasses = c("integer", "integer", "integer"), nrows=num_faces);

  ret_list = list();
  ret_list$vertices = data.matrix(vertices_df);
  ret_list$faces = data.matrix(faces_df) + 1L;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.patch", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ASCII patch file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in ASCII patch file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));
  }

  return(ret_list);
}
