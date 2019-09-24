#' @title Read file in FreeSurfer surface format
#'
#' @description Read vertex and face data  from a file in FreeSurfer binary surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'.
#'
#' @param filepath, string. Full path to the input curv file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @return data, vector of floats. The brain morphometry data, one value per vertex.
#'
#' @examples
#'     surface_file = system.file("extdata", "lh.white.gz",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     mesh = read.fs.surface(surface_file);
#'     cat(sprintf("Read data for %d vertices and %d faces. \n",
#'                             length(mesh$vertices), length(mesh$faces)));
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
    ret_list$mesh_face_type = "quads";
    num_vertices = fread3(fh);
    num_faces = fread3(fh);

    vertices = readBin(fh, integer(), size=2, n = num_vertices * 3, endian = "big");
    vertices = vertices / 100;
    vertices = matrix(vertices, nrow=num_vertices, ncol=3)

    faces = rep(0, num_faces * 4);
    faces = matrix(faces, nrow=num_faces, ncol=4)
    for (face_idx in 1:num_faces) {
      for (vertex_idx in 1:4) {
        faces[face_idx, vertex_idx] = fread3(fh);
      }
    }

  } else if(magic_byte == TRIS_MAGIC_FILE_TYPE_NUMBER) {
    ret_list$mesh_face_type = "tris";
    creation_date_text_line = readBin(fh, character());
    cat(sprintf("Creation date line: '%s'\n", creation_date_text_line))
    seek(fh, where=3, origin="current")
    info_text_line = readBin(fh, character());
    seek(fh, where=-5, origin="current")
    cat(sprintf("Info line: '%s'\n", info_text_line))

    num_vertices = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    num_faces = readBin(fh, integer(), size = 4, n = 1, endian = "big");
    cat(sprintf("Reading %d vertices and %d faces.\n", num_vertices, num_faces))

    cat(sprintf("Reading %d bytes of vertex data...\n", (num_vertices * 3L)))
    vertices = readBin(fh, numeric(), n = num_vertices * 3L, endian = "big");          # a vertex is made up of 3 float coordinates (x,y,z)
    if(length(vertices) != num_vertices) {
      warn(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertices))
    }
    vertices = matrix(vertices, nrow=num_vertices, ncol=3)

    cat(sprintf("Reading %d bytes of face data...\n", (num_faces * 3L)))
    faces = readBin(fh, integer(), size = 4, n = num_faces * 3L, endian = "big");   # a face is made of of 3 integers, which are vertex indices
    faces = matrix(faces, nrow=num_faces, ncol=3)

  } else {
    stop(sprintf("Magic number mismatch (%d != (%d || %d)). The given file '%s' is not a valid FreeSurfer surface format file in binary format. (Hint: This function is designed to read files like 'lh.white' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, TRIS_MAGIC_FILE_TYPE_NUMBER, QUAD_MAGIC_FILE_TYPE_NUMBER, filepath));

  }


  close(fh);


  ret_list$vertices = vertices;
  ret_list$faces = faces;
  return(ret_list);
}

