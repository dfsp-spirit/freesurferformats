#' @title Read file in FreeSurfer surface format
#'
#' @description Read vertex and face data  from a file in FreeSurfer binary surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'.
#'
#' @param filepath, string. Full path to the input curv file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software. "vertex_indices_fs": list of n integers, where n is the number of vertices. The FreeSurfer vertex indices for the vertices.
#'
#' @examples
#'     surface_file = system.file("extdata", "lh.white.gz",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'     cat(sprintf("Read data for %d vertices and %d faces. \n",
#'                             nrow(mesh$vertices), nrow(mesh$faces)));
#'
#' @export
read.fs.surface <- function(filepath) {
  TRIS_MAGIC_FILE_TYPE_NUMBER = 16777214;
  QUAD_MAGIC_FILE_TYPE_NUMBER = 16777215;

  if(guess.filename.is.gzipped(filepath)) {
    fh = gzfile(filepath, "rb");
  } else {
    fh = file(filepath, "rb");
  }

  ret_list = list();

  magic_byte = fread3(fh);
  if (magic_byte == QUAD_MAGIC_FILE_TYPE_NUMBER) {
    warning("Reading QUAD files in untested atm. Please use with care. This warning will be removed once the code has unit tests.")
    ret_list$mesh_face_type = "quads";

    num_vertices = fread3(fh);
    num_faces = fread3(fh);
    cat(sprintf("Reading surface file, expecting %d vertices and %d faces.\n", num_vertices, num_faces));

    ret_list$internal = list();
    ret_list$internal$num_vertices_expected = num_vertices;
    ret_list$internal$num_faces_expected = num_faces;


    num_vertex_coords = num_vertices * 3L;
    vertex_coords = readBin(fh, integer(), size=2L, n = num_vertex_coords, endian = "big");
    vertex_coords = vertex_coords / 100.;
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L);

    if(length(vertex_coords) != num_vertex_coords) {
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));
    }

    num_face_vertex_indices = num_faces * 4L;
    face_vertex_indices = rep(0, num_face_vertex_indices);
    faces = matrix(face_vertex_indices, nrow=num_faces, ncol=4L)
    for (face_idx in 1L:num_faces) {
      for (vertex_idx_in_face in 1L:4L) {
        global_vertex_idx = fread3(fh);
        faces[face_idx, vertex_idx_in_face] = global_vertex_idx;
      }
    }

  } else if(magic_byte == TRIS_MAGIC_FILE_TYPE_NUMBER) {
    ret_list$mesh_face_type = "tris";

    use_read_lines = FALSE;
    if (use_read_lines) {
      creation_date_text_line = readLines(fh, 1);
      cat(sprintf("creation_date_text_line= '%s'\n", creation_date_text_line))
      info_text_line = readLines(fh, 1);
      cat(sprintf("info_text_line= '%s'\n", info_text_line));
    } else {
      creation_date_text_line = readBin(fh, character(), endian = "big");
      cat(sprintf("creation_date_text_line= '%s'\n", creation_date_text_line))
      seek(fh, where=3, origin="current")
      info_text_line = readBin(fh, character(), endian = "big");
      cat(sprintf("info_text_line= '%s'\n", info_text_line));
      seek(fh, where=-5, origin="current") # skip string termination
    }


    #info_text_line = readBin(fh, character(), endian = "big");
    #seek(fh, where=-5, origin="current") # rewind

    ret_list$internal = list();
    ret_list$internal$creation_date_text_line = creation_date_text_line;
    #ret_list$internal$info_text_line = info_text_line;

    cur_pos = seek(fh, where=NA);
    cat(sprintf("At position %d before reading num_vertices.\n", cur_pos));

    num_vertices = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    num_faces = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    ret_list$internal$num_vertices_expected = num_vertices;
    ret_list$internal$num_faces_expected = num_faces;

    num_vertex_coords = num_vertices * 3L;
    vertex_coords = readBin(fh, numeric(), size = 4L, n = num_vertex_coords, endian = "big");          # a vertex is made up of 3 float coordinates (x,y,z)
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L);

    if(length(vertex_coords) != num_vertex_coords) {
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));
    }

    num_face_vertex_indices = num_faces * 3L;
    face_vertex_indices = readBin(fh, integer(), size = 4L, n = num_face_vertex_indices, endian = "big");   # a face is made of of 3 integers, which are vertex indices
    faces = matrix(face_vertex_indices, nrow=num_faces, ncol=3L);
    faces = faces + 1L;    # Increment indices by 1: GNU R uses 1-based indices.

    if(length(face_vertex_indices) != num_face_vertex_indices) {
      stop(sprintf("Mismatch in read vertex indices for faces: expected %d but received %d.\n", num_face_vertex_indices, length(face_vertex_indices)));
    }

  } else {
    stop(sprintf("Magic number mismatch (%d != (%d || %d)). The given file '%s' is not a valid FreeSurfer surface format file in binary format. (Hint: This function is designed to read files like 'lh.white' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, TRIS_MAGIC_FILE_TYPE_NUMBER, QUAD_MAGIC_FILE_TYPE_NUMBER, filepath));

  }


  close(fh);


  ret_list$vertices = vertices;
  ret_list$vertex_indices_fs = 0L:(nrow(vertices)-1)
  ret_list$faces = faces;
  return(ret_list);
}

