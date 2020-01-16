#' @title Read a surface, based on the file path without extension.
#'
#' @description Tries to read all files which can be constructed from the base path and the given extensions.
#'
#' @param filepath_noext character string, the full path to the input surface file without file extension.
#'
#' @param extensions vector of character strings, the file extensions to try.
#'
#' @param ... parameters passed on to \code{\link[freesurferformats]{read_nisurfacefile}}. Allows you to set the `methods`.
#'
#' @return an instance of `fs.surface`, read from the file. See \code{\link[freesurferformats]{read.fs.surface}} for details. If none of the reader methods succeed, an error is raised.
#'
#' @family mesh functions
#'
#' @examples
#' \donttest{
#'     surface_filepath_noext =
#'      paste(get_optional_data_filepath("subjects_dir/subject1/surf/"),
#'      'lh.white', sep="");
#'     mesh = read_nisurface(surface_filepath_noext);
#'     mesh;
#'  }
#'
#' @export
read_nisurface <- function(filepath_noext, extensions=c('', '.asc', '.gii'), ...) {
  surffile = readable.files(filepath_noext, precedence=extensions);
  return(read_nisurfacefile(surffile, ...));
}


#' @title S3 method to read a neuroimaging surface file.
#'
#' @description Tries to read the file with all implemented surface format reader methods. The file must exist. With the default settings, one can read files in the following surface formats: 1) FreeSurfer binary surface format (e.g., `surf/lh.white`). 2) FreeSurfer ASCII surface format (e.g., `surf/lh.white,asc`). 3) GIFTI surface format, only if package `gifti` is installed. See \code{\link[gifti]{read_gifti}} for details. Feel free to implement additional methods. Hint:keep in mind that they should return one-based indices.
#'
#' @param filepath character string, the full path to the input surface file.
#'
#' @param methods list of character strings, the formats to try. Each of these must have a function called \code{read_nisurface.<method>}, which must return an `fs.surface` instance on success.
#'
#' @param ... parameters passed on to the individual methods
#'
#' @return an instance of `fs.surface`, read from the file. See \code{\link[freesurferformats]{read.fs.surface}} for details. If none of the reader methods succeed, an error is raised.
#'
#' @family mesh functions
#'
#' @examples
#'     surface_file = system.file("extdata", "lh.tinysurface",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     mesh = read_nisurface(surface_file);
#'     mesh;
#'
#' @export
read_nisurfacefile <- function(filepath, methods=c('fsnative', 'fsascii', 'gifti'), ...) {
  if(!file.exists(filepath)) {
    stop(sprintf("Cannot read neuroimaging surface, file '%s' does not exist.\n", filepath));
  }

  class(filepath) <- c(methods, class(filepath));
  UseMethod('read_nisurfacefile', object = filepath);
}


#' @title Read a FreeSurfer ASCII surface file.
#'
#' @param filepath character string, the full path to the input surface file.
#'
#' @param ... parameters passed to \code{\link[freesurferformats]{read.fs.surface}}.
#'
#' @return an instance of `fs.surface`, read from the file. See \code{\link[freesurferformats]{read.fs.surface}} for details. If none of the reader methods succeed, an error is raised.
#'
#' @export
read_nisurfacefile.fsnative <- function(filepath, ...) {
  # try to read via read.fs.surface
  res <- tryCatch({
    freesurferformats::read.fs.surface(filepath, ...);
  }, error = function(e) {
    NULL;
  });

  # On success, return the surface.
  if(!is.null(res)){
    return(res);
  }

  # Failed, use the next read.ni.surface.* method
  NextMethod('read_nisurfacefile');
}


#' @title Read a FreeSurfer ASCII surface file.
#'
#' @param filepath character string, the full path to the input surface file.
#'
#' @param ... parameters passed to \code{\link[freesurferformats]{read.fs.surface.asc}}.
#'
#' @return an instance of `fs.surface`, read from the file. See \code{\link[freesurferformats]{read.fs.surface}} for details. If none of the reader methods succeed, an error is raised.
#'
#' @export
read_nisurfacefile.fsascii <- function(filepath, ...) {
  res <- tryCatch({
    freesurferformats::read.fs.surface.asc(filepath, ...);
  }, error = function(e) {
    NULL;
  });

  # On success, return the surface.
  if(!is.null(res)){
    return(res);
  }

  # Failed, use the next read.ni.surface.* method
  NextMethod('read_nisurfacefile');
}


#' @title Read a gifti file as a surface.
#'
#' @param filepath character string, the full path to the input surface file.
#'
#' @param ... ignored
#'
#' @return an instance of `fs.surface`, read from the file. See \code{\link[freesurferformats]{read.fs.surface}} for details. If none of the reader methods succeed, an error is raised.
#'
#' @export
read_nisurfacefile.gifti <- function(filepath, ...) {
  if (requireNamespace("gifti", quietly = TRUE)) {
    # Try to read via gifti package
    res <- tryCatch({
      gifti::read_gifti(filepath);
    }, error = function(e) {
      NULL;
    }, warning = function(w) {
      NULL
    });

  } else {   # Won't work without the 'gifti' package
    res = NULL;
  }

  # On success, return the result as a surface.
  if(!is.null(res)){
    ret_list = list("vertices"=res$data$pointset, "faces"=res$data$triangle + 1);
    class(ret_list) = c("fs.surface", class(ret_list));
    return(ret_list);
  }

  # Failed, use the next read.ni.surface.* method
  NextMethod('read_nisurfacefile');
}

#' @export
read_nisurfacefile.default <- function(filepath, ...) {
  stop(sprintf("Surface file '%s' could not be read with any of the available methods, format invalid or not supported.\n", filepath));
}


#' @title Read FreeSurfer ASCII format surface.
#'
#' @param filepath string. Full path to the input surface file in ASCII surface format.
#'
#' @param metadata named list of arbitrary metadata to store in the instance.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @family mesh functions
#'
#' @export
read.fs.surface.asc <- function(filepath, metadata=list()) {
  num_verts_and_faces_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts', 'num_faces'), colClasses = c("integer", "integer"));
  num_verts = num_verts_and_faces_df$num_verts[1];
  num_faces = num_verts_and_faces_df$num_faces[1];

  vertices_df = read.table(filepath, skip=2L, col.names = c('coord1', 'coord2', 'coord3', 'value'), colClasses = c("numeric", "numeric", "numeric", "numeric"), nrows=num_verts);

  faces_df = read.table(filepath, skip=2L + num_verts, col.names = c('vertex1', 'vertex2', 'vertex3', 'value'), colClasses = c("integer", "integer", "integer", "numeric"), nrows=num_faces);

  ret_list = list();
  ret_list$vertices = data.matrix(vertices_df[1:3]);
  ret_list$faces = data.matrix(faces_df[1:3]) + 1;  # the +1 is because the surface should use R indices (one-based)
  class(ret_list) = c("fs.surface", class(ret_list));

  if(nrow(ret_list$vertices) != num_verts) {
    stop(sprintf("Expected %d vertices in ASCII surface file '%s' from header, but received %d.\n", num_verts, filepath, nrow(ret_list$vertices)));
  }
  if(nrow(ret_list$faces) != num_faces) {
    stop(sprintf("Expected %d faces in ASCII surface file '%s' from header, but received %d.\n", num_faces, filepath, nrow(ret_list$faces)));
  }

  return(ret_list);
}


#' @title Read file in FreeSurfer surface format
#'
#' @description Read a brain surface mesh consisting of vertex and face data from a file in FreeSurfer binary surface format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.white'.
#'
#' @param filepath string. Full path to the input surface file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @param metadata named list of arbitrary metadata to store in the instance.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
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
read.fs.surface <- function(filepath, metadata=list()) {
  TRIS_MAGIC_FILE_TYPE_NUMBER = 16777214;
  QUAD_MAGIC_FILE_TYPE_NUMBER = 16777215;

  if(guess.filename.is.gzipped(filepath, gz_entensions=c(".asc"))) {
    return(read.fs.surface.asc(filepath, metadata));
  }

  if(guess.filename.is.gzipped(filepath)) {
    fh = gzfile(filepath, "rb");
  } else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list("metadata"=metadata);

  magic_byte = fread3(fh);
  if (magic_byte == QUAD_MAGIC_FILE_TYPE_NUMBER) {
    warning("Reading QUAD files in untested atm. Please use with care. This warning will be removed once we have an example input file and the code has unit tests.")
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
    vertices = matrix(vertex_coords, nrow=num_vertices, ncol=3L, byrow = TRUE);

    if(length(vertex_coords) != num_vertex_coords) {
      stop(sprintf("Mismatch in read vertex coordinates: expected %d but received %d.\n", num_vertex_coords, length(vertex_coords)));
    }

    num_face_vertex_indices = num_faces * 4L;
    face_vertex_indices = rep(0, num_face_vertex_indices);
    faces = matrix(face_vertex_indices, nrow=num_faces, ncol=4L, byrow = TRUE)
    for (face_idx in 1L:num_faces) {
      for (vertex_idx_in_face in 1L:4L) {
        global_vertex_idx = fread3(fh);
        faces[face_idx, vertex_idx_in_face] = global_vertex_idx;
      }
    }

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
    stop(sprintf("Magic number mismatch (%d != (%d || %d)). The given file '%s' is not a valid FreeSurfer surface format file in binary format. (Hint: This function is designed to read files like 'lh.white' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, TRIS_MAGIC_FILE_TYPE_NUMBER, QUAD_MAGIC_FILE_TYPE_NUMBER, filepath));
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


#' @title Check whether object is an fs.surface
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain surface (that is, has "fs.surface" amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.surface <- function(x) inherits(x, "fs.surface")


