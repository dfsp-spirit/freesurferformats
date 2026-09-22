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
#' @param format character string, the format to use. One of 'bin' for FreeSurfer binary surface format, 'asc' for FreeSurfer ASCII format, 'vtk' for VTK ASCII legacy format, 'ply' for Standford PLY format, 'off' for Object File Format, 'obj' for Wavefront object format, 'gii' for GIFTI format, 'mz3' for Surf-Ice MZ3 fomat, 'byu' for Brigham Young University (BYU) mesh format, 'stl' for the stereolithography (STL) format used for 3D printing, or 'auto' to derive the format from the file extension given in parameter 'filepath'. With 'auto', a path ending in '.asc' is interpreted as 'asc', a path ending in '.vtk' as vtk, and so on for the other formats. A path ending in '.stla' is interpreted as the ASCII variant of the STL format and one ending in '.stlb' or '.stl' as the binary variant. Everything not matching any of these is interpreted as 'bin', i.e., FreeSurfer binary surface format.
#'
#' @return character string, the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @family mesh functions
#' @family mesh export functions
#'
#'
#' @examples
#' \dontrun{
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface(tempfile(), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface <- function(filepath, vertex_coords, faces, format = "auto") {
  if (!(format %in% c("auto", "bin", "asc", "vtk", "obj", "off", "ply", "gii", "mz3", "byu", "stl"))) {
    stop("Format must be one of c('auto', 'bin', 'asc', 'vtk', 'obj', 'off', 'ply', 'gii', 'mz3', 'byu', 'stl').")
  }

  if (!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces)

  if (format == "asc" | (format == "auto" & filepath.ends.with(filepath, c(".asc")))) {
    return(write.fs.surface.asc(filepath, vertex_coords, faces))
  }

  if (format == "vtk" | (format == "auto" & filepath.ends.with(filepath, c(".vtk")))) {
    return(write.fs.surface.vtk(filepath, vertex_coords, faces))
  }

  if (format == "obj" | (format == "auto" & filepath.ends.with(filepath, c(".obj")))) {
    return(write.fs.surface.obj(filepath, vertex_coords, faces))
  }

  if (format == "off" | (format == "auto" & filepath.ends.with(filepath, c(".off")))) {
    return(write.fs.surface.off(filepath, vertex_coords, faces))
  }

  if (format == "ply" | (format == "auto" & filepath.ends.with(filepath, c(".ply")))) {
    return(write.fs.surface.ply(filepath, vertex_coords, faces))
  }

  if (format == "gii" | (format == "auto" & filepath.ends.with(filepath, c(".gii")))) {
    return(write.fs.surface.gii(filepath, vertex_coords, faces))
  }

  if (format == "mz3" | (format == "auto" & filepath.ends.with(filepath, c(".mz3")))) {
    return(write.fs.surface.mz3(filepath, vertex_coords, faces))
  }

  if (format == "byu" | (format == "auto" & filepath.ends.with(filepath, c(".byu")))) {
    return(write.fs.surface.byu(filepath, vertex_coords, faces))
  }

  if (format == "stl" | (format == "auto" & filepath.ends.with(filepath, c(".stl", ".stla", ".stlb")))) {
    # The STL format has no format marker that a reader could use, so the
    # extension decides here: '.stla' is the ASCII variant and '.stl' and
    # '.stlb' are the binary variant (as in the writer below, the binary one is
    # the default, because it is much smaller and it is what most tools write).
    ascii <- filepath.ends.with(filepath, c(".stla"))
    return(write.fs.surface.stl(filepath, vertex_coords, faces, ascii = ascii))
  }

  TRIS_MAGIC_FILE_TYPE_NUMBER <- 16777214L
  OLD_QUAD_MAGIC_FILE_TYPE_NUMBER <- 16777215L
  NEW_QUAD_MAGIC_FILE_TYPE_NUMBER <- 16777213L

  num_faces_with_index_zero <- sum(faces == 0)
  if (num_faces_with_index_zero > 0) {
    stop(sprintf("The vertex indices defining the faces must be 1-based (GNU R style). That means the value 0 must not occur in the matrix 'faces', but %d of the %d face vertex indices have this value (most likely you will need to add 1 to all values in 'faces').", num_faces_with_index_zero, length(faces)))
  }

  faces <- faces - 1L

  if (typeof(faces) != "integer") {
    stop(sprintf("The type of the faces matrix must be 'integer' but is '%s'.", typeof(faces)))
  }


  MAGIC_FILE_TYPE_NUMBER <- TRIS_MAGIC_FILE_TYPE_NUMBER
  format_written <- "tris"
  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)

  if (guess.filename.is.gzipped(filepath, gz_extensions = c(".gz"))) {
    fh <- gzfile(filepath, "wb")
  } else {
    fh <- file(filepath, "wb", blocking = TRUE)
  }

  fwrite3(fh, MAGIC_FILE_TYPE_NUMBER)

  creation_date_line <- "Created by anonymous on a perfect day.\n"
  writeLines(creation_date_line, con = fh)

  writeBin(as.integer(num_verts), fh, size = 4, endian = "big")
  writeBin(as.integer(num_faces), fh, size = 4, endian = "big")

  # write the data itself: vertex coords
  writeBin(c(t(vertex_coords)), fh, size = 4, endian = "big")

  # write vertex indices making up a face
  writeBin(c(t(faces)), fh, size = 4, endian = "big")
  close(fh)

  return(invisible(format_written))
}


#' @keywords internal
check.verts.faces <- function(vertex_coords, faces) {
  if (ncol(vertex_coords) != 3L) {
    stop("Parameter 'vertex_coords' must be a matrix with 3 columns (the x, y, z coords of the vertices).")
  }
  if (ncol(faces) != 3L) {
    stop("Parameter 'faces' must be a matrix with 3 columns (the indices of the vertices making up the faces).")
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.asc(tempfile(fileext = ".asc"), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface.asc <- function(filepath, vertex_coords, faces) {
  check.verts.faces(vertex_coords, faces)

  # Write the first comment line and the 2nd line containing the number of vertices in the label
  fh <- file(filepath)
  writeLines(c("#!ascii version of surface", sprintf("%d %d", nrow(vertex_coords), nrow(faces))), fh)
  close(fh)

  if (ncol(vertex_coords) == 3) {
    vertex_coords <- cbind(vertex_coords, 0L) # add isInPatch flag (can be 0 or 1), aka 'ripflag'
  }

  faces <- faces - 1L
  if (ncol(faces) == 3) {
    faces <- cbind(faces, 0L) # add isInPatch flag (can be 0 or 1)
  }

  # Append the vertex data
  write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  # Append the face data
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible("tris"))
}


#' @title Write mesh to file in VTK legacy format
#'
#' @description The VTK legacy format is the plain text/binary format that is
#'   supported by all versions of the VTK library; the XML based VTK format
#'   (.vtp) is not the same thing. Two variants of the file layout exist and are
#'   both still written by software in use today: the old layout introduced in
#'   VTK 4.2, and the layout that VTK produces since version 5.1 (released 2015).
#'   The parameter 'version' selects which one to write.
#'
#' @param filepath string. Full path to the output surface file, should end with '.vtk', but that is not enforced.
#'
#' @param vertex_coords n x 3 matrix of doubles. Each row defined the x,y,z coords for a vertex.
#'
#' @param faces n x 3 matrix of integers. Each row defined the 3 vertex indices that make up the face. WARNING: Vertex indices should be given in R-style, i.e., the index of the first vertex is 1. However, they will be written in VTK style, i.e., all indices will have 1 substracted, so that the index of the first vertex will be zero.
#'
#' @param version double, the VTK version whose file layout to write. Either 4.2 (the default) or 5.1. Version 4.2 writes the cell array layout that every VTK version can read, version 5.1 writes the `OFFSETS`/`CONNECTIVITY` layout that VTK itself has been producing since 2015. Only change this if you know that the software you hand the file to requires the newer layout.
#'
#' @param binary logical, whether to write the data in binary form instead of the ASCII text form. Binary files are much smaller and much faster to read and write, but they are not human readable. Defaults to FALSE.
#'
#' @return string the format that was written. One of "tris" or "quads". Currently only triangular meshes are supported, so always 'tris'.
#'
#' @note Binary data in the VTK legacy format is always big endian, the format has no way of expressing a different byte order. The vertex coordinates are written as single precision (4 byte) floats in both encodings, which is what VTK itself does.
#'
#' @family mesh functions
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.vtk(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces)
#' # Or as a binary file using the layout of VTK 5.1:
#' write.fs.surface.vtk(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces,
#'   version = 5.1, binary = TRUE
#' )
#' }
#'
#' @export
write.fs.surface.vtk <- function(filepath, vertex_coords, faces, version = 4.2, binary = FALSE) {
  check.verts.faces(vertex_coords, faces)

  if (!is.logical(binary) || length(binary) != 1L || is.na(binary)) {
    stop("Parameter 'binary' must be a single logical value (TRUE or FALSE).\n")
  }

  version_num <- suppressWarnings(as.numeric(version))
  if (length(version_num) != 1L || is.na(version_num)) {
    stop("Parameter 'version' must be a single number, either 4.2 or 5.1.\n")
  }
  version <- sprintf("%.1f", version_num)
  if (!(version %in% c("4.2", "5.1"))) {
    stop(sprintf(
      "Parameter 'version' must be either 4.2 or 5.1, but it is '%s'. Version 4.2 writes the cell array layout that all VTK versions can read, version 5.1 writes the layout that VTK itself has been producing since 2015.\n",
      version
    ))
  }

  if (any(faces == 0L)) {
    stop("The vertex indices defining the faces must be 1-based (GNU R style). That means the value 0 must not occur in the matrix 'faces'. To write a mesh whose faces use 0-based indices, add 1 to all values in 'faces' first.\n")
  }
  if (!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }

  num_faces <- nrow(faces)
  if (num_faces > (.Machine$integer.max %/% 3L)) {
    stop(sprintf("The mesh has %d faces, which is too many for the VTK legacy format.\n", num_faces))
  }

  # From R (one-based) to VTK (zero-based) vertex indices.
  faces <- faces - 1L

  con <- file(filepath, "wb") # in binary mode, so that writeLines() does not translate line endings
  on.exit(
    {
      close(con)
    },
    add = TRUE
  )

  writeLines(c(
    sprintf("# vtk DataFile Version %s", version),
    "fsbrain output",
    if (binary) "BINARY" else "ASCII",
    "DATASET POLYDATA"
  ), con)

  if (binary) {
    vtk.write.surface.binary(con, vertex_coords, faces, version)
  } else {
    vtk.write.surface.ascii(con, vertex_coords, faces, version)
  }

  return(invisible("tris"))
}


#' @title Write the sections of a triangular mesh in VTK ASCII format.
#'
#' @param con a connection opened for writing.
#'
#' @param vertex_coords n x 3 matrix of doubles, the vertex coordinates.
#'
#' @param faces n x 3 matrix of integers, the vertex indices of the faces,
#'   already converted to zero-based indices.
#'
#' @param version character string, either '4.2' or '5.1'.
#'
#' @return \code{NULL}, invisibly.
#'
#' @keywords internal
vtk.write.surface.ascii <- function(con, vertex_coords, faces, version) {
  writeLines(sprintf("POINTS %d float", nrow(vertex_coords)), con)
  writeLines(sprintf("%.15g %.15g %.15g", vertex_coords[, 1L], vertex_coords[, 2L], vertex_coords[, 3L]), con)

  num_faces <- nrow(faces)
  if (version == "4.2") {
    # Old layout: the cell array holds the vertex count of every cell followed by
    # its vertex indices, so it is four times as long as the number of cells.
    writeLines(sprintf("POLYGONS %.0f %.0f", num_faces, num_faces * 4), con)
    writeLines(sprintf("3 %d %d %d", faces[, 1L], faces[, 2L], faces[, 3L]), con)
  } else {
    # New layout: separate OFFSETS and CONNECTIVITY arrays, the two numbers of
    # the section header line are the lengths of those two arrays.
    writeLines(sprintf("POLYGONS %.0f %.0f", num_faces + 1, num_faces * 3), con)
    writeLines("OFFSETS vtktypeint64", con)
    writeLines(sprintf("%.0f", seq(from = 0, by = 3, length.out = num_faces + 1L)), con)
    writeLines("CONNECTIVITY vtktypeint64", con)
    writeLines(sprintf("%d %d %d", faces[, 1L], faces[, 2L], faces[, 3L]), con)
  }
  return(invisible(NULL))
}


#' @title Write the sections of a triangular mesh in binary VTK format.
#'
#' @param con a connection opened for binary writing.
#'
#' @param vertex_coords n x 3 matrix of doubles, the vertex coordinates.
#'
#' @param faces n x 3 matrix of integers, the vertex indices of the faces,
#'   already converted to zero-based indices.
#'
#' @param version character string, either '4.2' or '5.1'.
#'
#' @return \code{NULL}, invisibly.
#'
#' @note Binary data in the VTK legacy format is always big endian, the format
#'   has no way of expressing a different byte order.
#'
#' @keywords internal
vtk.write.surface.binary <- function(con, vertex_coords, faces, version) {
  writeLines(sprintf("POINTS %d float", nrow(vertex_coords)), con)
  writeBin(as.numeric(t(vertex_coords)), con, size = 4L, endian = "big")

  num_faces <- nrow(faces)
  if (version == "4.2") {
    writeLines(sprintf("POLYGONS %.0f %.0f", num_faces, num_faces * 4), con)
    if (num_faces > 0L) {
      # Every cell is a 4 byte vertex count followed by that many 4 byte vertex indices.
      records <- cbind(3L, faces)
      writeBin(as.integer(t(records)), con, size = 4L, endian = "big")
    }
  } else {
    writeLines(sprintf("POLYGONS %.0f %.0f", num_faces + 1, num_faces * 3), con)
    writeLines("OFFSETS vtktypeint64", con)
    writeBin(as.integer(seq(from = 0, by = 3, length.out = num_faces + 1L)), con, size = 8L, endian = "big")
    writeLines("CONNECTIVITY vtktypeint64", con)
    if (num_faces > 0L) {
      writeBin(as.integer(t(faces)), con, size = 8L, endian = "big")
    }
  }
  return(invisible(NULL))
}


#' @title Write mesh to file in STL format (ASCII or binary).
#'
#' @description The STL format (stereolithography, the format used for 3D
#'   printing) stores a triangular mesh as a list of triangles, each with its
#'   vertex coordinates repeated and with a normal vector, instead of storing a
#'   vertex list and indices into it. Both the ASCII and the binary version of
#'   the format are written by this function, the binary one being the default
#'   since it is much smaller and it is what most software uses. The resulting
#'   files can be read back with \code{\link{read.fs.surface.stl}} and are
#'   accepted by mesh viewers and slicers.
#'
#' @param filepath character string, the path of the file to write.
#'
#' @param vertex_coords n x 3 matrix of doubles, the vertex coordinates.
#'
#' @param faces n x 3 matrix of integers, the vertex indices of the triangles.
#'   The STL format has no support for polygons with more than 3 vertices, so a
#'   quad mesh has to be converted first with \code{\link{faces.quad.to.tris}}.
#'
#' @param ascii logical, whether to write the ASCII version of the format. The
#'   default is the binary version, which is smaller by a factor of about 5 and
#'   which is what most mesh processing software writes. Use the ASCII version if
#'   the file has to be readable by humans or by software that supports only the
#'   ASCII variant.
#'
#' @param solid_name character string, the name of the mesh. Only used in the
#'   ASCII version, where the format requires the name in the first and the last
#'   line of the file.
#'
#' @return character string, the format that was written: 'tris'.
#'
#' @family mesh export functions
#'
#' @examples
#' \dontrun{
#' # Write a mesh as binary and as ASCII STL:
#' mesh <- read.fs.surface(system.file("extdata", "cube.stl", package = "freesurferformats"));
#' write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces);
#' write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces, ascii = TRUE);
#'
#' # The file format is also chosen by the file name when using the generic
#' # writer:
#' write.fs.surface(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces);
#' }
#'
#' @note The normals of the triangles are computed from the vertex coordinates
#'   (the STL format stores them, but no reader has to trust them). A degenerate
#'   triangle, i.e., one whose vertices are collinear or identical, has no
#'   normal, so a zero vector is written for it.
#'
#' @note An indexed mesh is stored as a polygon soup in an STL file: every
#'   triangle repeats the coordinates of its vertices. Reading such a file back
#'   with \code{\link{read.fs.surface.stl}} merges the repeated vertices again
#'   (using the \code{digits} precision of that function), so a round trip
#'   through an STL file preserves the geometry of the mesh, but not the order or
#'   the count of the vertices in the vertex list.
#'
#' @export
write.fs.surface.stl <- function(filepath, vertex_coords, faces, ascii = FALSE, solid_name = "mesh") {
  if (!is.logical(ascii) || length(ascii) != 1L || is.na(ascii)) {
    stop("Parameter 'ascii' must be a single logical value (TRUE or FALSE).\n")
  }
  if (ncol(faces) == 4L) {
    stop(paste0("The STL format stores only triangular faces, but the 'faces' parameter defines quadrangular faces. ",
                "Convert the mesh with 'faces.quad.to.tris' first.\n"));
  }
  check.verts.faces(vertex_coords, faces)

  if (any(faces == 0L)) {
    stop(paste0("The vertex indices defining the faces must be 1-based (GNU R style). That means the value 0 must not ",
                "occur in the matrix 'faces', but ", sum(faces == 0L), " of the ", length(faces), " face vertex indices ",
                "have this value (most likely you will need to add 1 to all values in 'faces').\n"));
  }

  num_faces <- nrow(faces);
  face_normals <- mesh.face.normals(vertex_coords, faces);

  con <- file(filepath, "wb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  if (ascii) {
    write.stl.ascii(con, vertex_coords, faces, face_normals, solid_name);
  } else {
    write.stl.binary(con, vertex_coords, faces, face_normals);
  }

  return(invisible("tris"));
}


#' @title Compute the normals of the triangles of a mesh.
#'
#' @description The normal of a triangle is the unit vector orthogonal to its
#'   plane, it is computed as the normalized cross product of two of its edges.
#'   The STL format stores one normal per triangle, and the value is computed
#'   from the geometry instead of being taken from the data, since a mesh in
#'   index representation does not store normals at all.
#'
#' @param vertex_coords n x 3 matrix of doubles, the vertex coordinates.
#'
#' @param faces n x 3 matrix of integers, the vertex indices of the triangles.
#'
#' @return n x 3 matrix of doubles, the normalized normal of every face. Rows of
#'   degenerate triangles (whose 3 vertices lie on one line, which includes
#'   triangles with repeated vertices) are zero vectors, since such triangles
#'   have no plane and hence no normal.
#'
#' @keywords internal
mesh.face.normals <- function(vertex_coords, faces) {
  v1 <- vertex_coords[faces[, 1L], , drop = FALSE];
  v2 <- vertex_coords[faces[, 2L], , drop = FALSE];
  v3 <- vertex_coords[faces[, 3L], , drop = FALSE];

  edge1 <- v2 - v1;
  edge2 <- v3 - v1;

  normals <- cbind(edge1[, 2L] * edge2[, 3L] - edge1[, 3L] * edge2[, 2L],
                   edge1[, 3L] * edge2[, 1L] - edge1[, 1L] * edge2[, 3L],
                   edge1[, 1L] * edge2[, 2L] - edge1[, 2L] * edge2[, 1L]);

  lengths <- sqrt(rowSums(normals^2));
  # Dividing a matrix by a vector of its row count divides row-wise (R recycles
  # the shorter argument column by column).
  degenerate <- lengths == 0;
  lengths[degenerate] <- 1;
  normals <- normals / lengths;
  normals[degenerate, ] <- 0;
  return(normals);
}


#' @title Write the sections of a triangular mesh in ASCII STL format.
#'
#' @description Writes the 'solid' block of the ASCII variant of the STL format,
#'   with 7 lines per face ('facet normal', 'outer loop', 3 'vertex' lines,
#'   'endloop', 'endfacet'). This is the layout that
#'   \code{\link{read.fs.surface.stl.ascii}} and other STL readers expect.
#'
#' @param con a connection opened in binary write mode, the file is written as
#'   text through it.
#'
#' @param vertex_coords n x 3 matrix of doubles, the vertex coordinates.
#'
#' @param faces n x 3 matrix of integers, the vertex indices of the triangles.
#'
#' @param face_normals n x 3 matrix of doubles, the normals of the faces, see
#'   \code{\link{mesh.face.normals}}.
#'
#' @param solid_name character string, the name of the mesh.
#'
#' @return \code{NULL}, invisibly. The data are written to \code{con}.
#'
#' @keywords internal
write.stl.ascii <- function(con, vertex_coords, faces, face_normals, solid_name = "mesh") {
  num_faces <- nrow(faces);
  solid_name <- as.character(solid_name)[1L];

  writeLines(sprintf("solid %s", solid_name), con);

  if (num_faces > 0L) {
    # The faces are written in chunks to keep the number of sprintf calls per
    # call and the temporary memory bounded, this does not change the output.
    chunk_faces <- 10000L;
    written <- 0L;
    while (written < num_faces) {
      face_indices <- seq.int(written + 1L, min(written + chunk_faces, num_faces));
      chunk_normals <- face_normals[face_indices, , drop = FALSE];
      chunk_vertices <- vertex_coords[as.vector(t(faces[face_indices, , drop = FALSE])), , drop = FALSE];

      vertex_lines <- sprintf("    vertex %.6f %.6f %.6f", chunk_vertices[, 1L], chunk_vertices[, 2L],
                              chunk_vertices[, 3L]);
      # The 3 vertex lines of a face are 3 consecutive entries of vertex_lines.
      vertex_lines <- matrix(vertex_lines, ncol = 3L, byrow = TRUE);

      lines <- character(7L * length(face_indices));
      lines[seq.int(1L, length(lines), by = 7L)] <- sprintf("facet normal %.6f %.6f %.6f", chunk_normals[, 1L],
                                                            chunk_normals[, 2L], chunk_normals[, 3L]);
      lines[seq.int(2L, length(lines), by = 7L)] <- "  outer loop";
      lines[seq.int(3L, length(lines), by = 7L)] <- vertex_lines[, 1L];
      lines[seq.int(4L, length(lines), by = 7L)] <- vertex_lines[, 2L];
      lines[seq.int(5L, length(lines), by = 7L)] <- vertex_lines[, 3L];
      lines[seq.int(6L, length(lines), by = 7L)] <- "  endloop";
      lines[seq.int(7L, length(lines), by = 7L)] <- "endfacet";

      writeLines(lines, con);
      written <- written + length(face_indices);
    }
  }

  writeLines(sprintf("endsolid %s", solid_name), con);
  return(invisible(NULL));
}


#' @title Write the sections of a triangular mesh in binary STL format.
#'
#' @description Writes the binary variant of the STL format: an 80 byte header,
#'   a 4 byte face count, and then 50 bytes per face (3 float32 values for the
#'   face normal, 9 float32 values for the 3 vertex coordinates, and a zero
#'   uint16 attribute byte count). All values are little endian, as the format
#'   requires. Note that the header must not start with the string 'solid',
#'   which is how readers tell the ASCII and the binary variant apart.
#'
#' @inheritParams write.stl.ascii
#'
#' @return \code{NULL}, invisibly. The data are written to \code{con}.
#'
#' @keywords internal
write.stl.binary <- function(con, vertex_coords, faces, face_normals) {
  num_faces <- nrow(faces);

  header_text <- sprintf("Binary STL file written by the freesurferformats R package, %d triangular faces.", num_faces);
  header_bytes <- charToRaw(substr(header_text, 1L, 80L));
  header_bytes <- c(header_bytes, raw(80L - length(header_bytes)));
  writeBin(header_bytes, con);

  writeBin(as.integer(num_faces), con, size = 4L, endian = "little");

  if (num_faces > 0L) {
    chunk_faces <- 10000L;
    written <- 0L;
    while (written < num_faces) {
      face_indices <- seq.int(written + 1L, min(written + chunk_faces, num_faces));
      chunk_size <- length(face_indices);

      # The 12 float32 values of a face, in file order: normal, then the 3
      # vertices. Serializing to a raw vector and re-arranging the bytes allows
      # writing a whole chunk with a single writeBin call.
      values <- cbind(face_normals[face_indices, , drop = FALSE],
                      matrix(as.vector(t(vertex_coords[as.vector(t(faces[face_indices, , drop = FALSE])), , drop = FALSE])),
                             ncol = 9L, byrow = TRUE));
      value_bytes <- writeBin(as.numeric(t(values)), raw(), size = 4L, endian = "little");

      # One face is 50 bytes: 48 of them hold floats, the remaining 2 are the
      # uint16 attribute byte count, which is always zero ('no additional data').
      out <- matrix(raw(50L), nrow = 50L, ncol = chunk_size); # in R, a raw matrix is initialized to zeros.
      out[seq_len(48L), ] <- matrix(value_bytes, nrow = 48L);
      writeBin(as.vector(out), con);

      written <- written + chunk_size;
    }
  }

  return(invisible(NULL));
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.obj(tempfile(fileext = ".obj"), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface.obj <- function(filepath, vertex_coords, faces, vertex_colors = NULL) {
  check.verts.faces(vertex_coords, faces)

  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)

  # Write the vertex data
  vs <- matrix(rep("v", num_verts), ncol = 1L)
  verts <- cbind(vs, vertex_coords)

  use_vertex_colors <- !is.null(vertex_colors)
  if (use_vertex_colors) {
    if (is.character(vertex_colors)) {
      vertex_colors <- t(grDevices::col2rgb(vertex_colors, alpha = FALSE))
    }
    if ((!is.integer(vertex_colors)) | ncol(vertex_colors) != 3L) {
      stop("Parameter 'vertex_colors' must be a matrix of integers with 3 columns (RGB) in range 0-255.")
    }
    vertex_data <- data.frame(verts)
    vertex_colors_df <- data.frame(vertex_colors / 255.0)
    if (nrow(vertex_data) != nrow(vertex_colors_df)) {
      stop(sprintf("Data mismatch, received %d vertices but %d vertex colors.\n", nrow(vertex_data), nrow(vertex_colors_df)))
    }
    colnames(vertex_colors_df) <- c("r", "g", "b")
    verts <- cbind(vertex_data, vertex_colors_df)
  }

  write.table(verts, file = filepath, append = FALSE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  # Append the face data
  # Note that we do not shift the index, the format uses 1-based indices like R
  fs <- matrix(rep("f", num_faces), ncol = 1L)
  faces <- cbind(fs, faces)
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible("tris"))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.off(tempfile(fileext = ".off"), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface.off <- function(filepath, vertex_coords, faces) {
  return(write.fs.surface.off.ply2(filepath, vertex_coords, faces, format = "off"))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.off(tempfile(fileext = ".off"), mesh$vertices, mesh$faces)
#' }
#'
#' @keywords internal
write.fs.surface.off.ply2 <- function(filepath, vertex_coords, faces, format) {
  if (!format %in% c("ply2", "off")) {
    stop("Format must be 'ply2' or 'off'.")
  }

  check.verts.faces(vertex_coords, faces)

  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)

  fh <- file(filepath, "w")

  # write header
  if (format == "off") {
    count_line <- sprintf("%d %d %d", num_verts, num_faces, 0L)
    writeLines(c("# OFF", count_line), fh)
  } else {
    vertex_count_line <- sprintf("%d", num_verts)
    face_count_line <- sprintf("%d", num_faces)
    writeLines(c(vertex_count_line, face_count_line), fh)
  }
  close(fh)

  # Append the vertex data
  write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  # Append the face data
  faces <- faces - 1L # shift index to 0-based
  faces <- cbind(3L, faces) # each line starts with the number of verts in the face
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible("tris"))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.ply2(tempfile(fileext = ".ply2"), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface.ply2 <- function(filepath, vertex_coords, faces) {
  return(write.fs.surface.off.ply2(filepath, vertex_coords, faces, format = "ply2"))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices, mesh$faces)
#' # save a version with RGBA vertex colors
#' vertex_colors <- matrix(rep(82L, 5 * 4), ncol = 4)
#' write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices,
#'   mesh$faces,
#'   vertex_colors = vertex_colors
#' )
#' }
#'
#' @export
#' @importFrom grDevices col2rgb
write.fs.surface.ply <- function(filepath, vertex_coords, faces, vertex_colors = NULL) {
  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)

  check.verts.faces(vertex_coords, faces)

  fh <- file(filepath, "w")

  use_vertex_colors <- !is.null(vertex_colors)

  # write header
  header_lines <- ply.header.lines(num_verts, num_faces, use_vertex_colors)
  writeLines(header_lines, fh)
  close(fh)

  # Append the vertex data
  if (use_vertex_colors) {
    if (is.character(vertex_colors)) {
      vertex_colors <- t(grDevices::col2rgb(vertex_colors, alpha = TRUE))
    }
    if ((!is.integer(vertex_colors)) | ncol(vertex_colors) != 4L) {
      stop("Parameter 'vertex_colors' must be a matrix of integers with 4 columns (RGBA) in range 0-255.")
    }
    vertex_data <- data.frame(vertex_coords)
    vertex_colors_df <- data.frame(vertex_colors)
    if (nrow(vertex_data) != nrow(vertex_colors_df)) {
      stop(sprintf("Data mismatch, received %d vertices but %d vertex colors.\n", nrow(vertex_data), nrow(vertex_colors_df)))
    }
    colnames(vertex_colors_df) <- c("r", "g", "b", "a")
    vertex_data <- cbind(vertex_data, vertex_colors_df)
    write.table(vertex_data, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
  } else {
    write.table(vertex_coords, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
  }


  # Append the face data
  faces <- faces - 1L # shift index to 0-based
  faces <- cbind(3L, faces) # each line starts with the number of verts in the face
  write.table(faces, file = filepath, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible("tris"))
}


#' @title Generate PLY format header lines
#' @keywords internal
ply.header.lines <- function(num_verts, num_faces, use_vertex_colors) {
  header_top <- c("ply", "format ascii 1.0")
  header_verts <- c(sprintf("element vertex %d", num_verts), "property float x", "property float y", "property float z")
  header_vertex_colors <- c("property uchar red", "property uchar green", "property uchar blue", "property uchar alpha")

  header_faces <- c(sprintf("element face %d", num_faces), "property list uchar int vertex_indices")
  header_end <- "end_header"

  if (use_vertex_colors) {
    return(c(header_top, header_verts, header_vertex_colors, header_faces, header_end))
  } else {
    return(c(header_top, header_verts, header_faces, header_end))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.gii(tempfile(fileext = ".gii"), mesh$vertices, mesh$faces)
#' }
#'
#' @export
write.fs.surface.gii <- function(filepath, vertex_coords, faces) {
  if (!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces)
  my_data_sets <- list(vertex_coords, faces - 1L)
  xmltree <- gifti_xml(my_data_sets, datatype = c("NIFTI_TYPE_FLOAT32", "NIFTI_TYPE_INT32"), intent = c("NIFTI_INTENT_POINTSET", "NIFTI_INTENT_TRIANGLE"))
  # xml2::xml_validate(xmltree, xml2::read_xml("https://www.nitrc.org/frs/download.php/158/gifti.xsd"));
  gifti_xml_write(filepath, xmltree)
  return(invisible("tris"))
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.mz3(tempfile(fileext = ".mz3"), mesh$vertices, mesh$faces)
#' }
#'
#' @note This format is used by the surf-ice renderer. The format spec is at https://github.com/neurolabusc/surf-ice/tree/master/mz3.
#'
#' @export
write.fs.surface.mz3 <- function(filepath, vertex_coords, faces, gzipped = TRUE) {
  if (!identical(storage.mode(faces), "integer")) {
    storage.mode(faces) <- "integer"
  }
  check.verts.faces(vertex_coords, faces)

  faces <- faces - 1L

  format_written <- "tris"
  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)
  num_skip <- 0L

  if (gzipped) {
    fh <- gzfile(filepath, "wb")
  } else {
    fh <- file(filepath, "wb", blocking = TRUE)
  }

  magic_code <- 23117L
  attr <- 3L # verts + faces
  writeBin(as.integer(magic_code), fh, size = 2, endian = "little")
  writeBin(as.integer(attr), fh, size = 2, endian = "little")

  writeBin(as.integer(num_faces), fh, size = 4, endian = "little")
  writeBin(as.integer(num_verts), fh, size = 4, endian = "little")
  writeBin(as.integer(num_skip), fh, size = 4, endian = "little")

  # header done, now write the data itself.

  # write vertex indices making up a face
  writeBin(c(t(faces)), fh, size = 4, endian = "little")

  # write vertex coords
  writeBin(c(t(vertex_coords)), fh, size = 4, endian = "little")
  close(fh)

  return(invisible(format_written))
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
fixed.vec.format.int <- function(vdata, num_chars_per_entry, max_entries_per_line = NULL, align_right = TRUE) {
  num_chars_per_entry <- as.integer(num_chars_per_entry)
  if (align_right) {
    format_string <- sprintf("%% %dd", num_chars_per_entry)
  } else {
    format_string <- sprintf("%% -%dd", num_chars_per_entry)
  }
  return(fixed.format.lines(vdata, format_string, max_entries_per_line = max_entries_per_line))
}

#' @keywords internal
fixed.format.lines <- function(vdata, format_string, max_entries_per_line = NULL) {
  if (is.null(max_entries_per_line)) {
    return(paste(sprintf(format_string, vdata), collapse = ""))
  } else {
    result_string <- NULL
    num_left <- length(vdata)
    start_idx <- 1L
    while (num_left > 0L) {
      if (num_left >= max_entries_per_line) {
        end_idx <- start_idx + max_entries_per_line - 1L
      } else {
        end_idx <- length(vdata)
      }
      num_written <- end_idx - start_idx + 1L
      this_line <- paste(sprintf(format_string, vdata[start_idx:end_idx]), collapse = "")
      result_string <- c(result_string, this_line)
      num_left <- num_left - num_written
      start_idx <- end_idx + 1L
    }
    return(result_string)
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
#' # Read a surface from a file:
#' surface_file <- system.file("extdata", "lh.tinysurface",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' mesh <- read.fs.surface(surface_file)
#' # Now save it:
#' write.fs.surface.byu(tempfile(fileext = ".byu"), mesh$vertices, mesh$faces)
#' }
#'
#' @note This is a fixed field length ASCII format. Keep in mind that the BYU format expects the coordinates to be in the cube -1 to +1 on all three axes.
#'
#' @export
write.fs.surface.byu <- function(filepath, vertex_coords, faces) {
  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)

  check.verts.faces(vertex_coords, faces)

  # The BYU format expects the coordinates to be in the cube -1 to +1 on all axes. Warn if that is not the case.
  coord_min <- min(min(vertex_coords[, 1]), min(vertex_coords[, 2]), min(vertex_coords[, 3]))
  coord_max <- max(max(vertex_coords[, 1]), max(vertex_coords[, 2]), max(vertex_coords[, 3]))
  if (coord_min < -1.0 | coord_max > 1.0) {
    stop(sprintf("Exported BYU mesh contains vertex coordinates outside of expected range -1 to 1 (coord range of mesh is %f to %f). Consider rescaling.\n", coord_min, coord_max))
  }

  fh <- file(filepath, "w")

  # write header
  num_meshes <- 1L
  header_data <- c(num_meshes, num_verts, num_faces, (num_faces * 3L), 0L)
  header_line <- fixed.vec.format.int(header_data, num_chars_per_entry = 6L)
  # write the lines identifying the start and stop of the meshes in the faces list. Only one mesh in our case, that spans the entire list.
  part_line <- fixed.vec.format.int(c(1L, (num_faces * 3L)), num_chars_per_entry = 6L)
  writeLines(c(header_line, part_line), fh)

  # write the vertex coordinates
  vertex_coords_vec <- as.double(t(vertex_coords))
  vertex_lines <- fixed.format.lines(vertex_coords_vec, format_string = "% 1.5e", max_entries_per_line = 6L)
  writeLines(vertex_lines, fh)

  # Write faces.
  # Turn matrix of vertex indices into a vector and make the index of the last vertex of each face negative:
  face_vertex_indices <- as.integer(t(faces))
  last_vertices_of_faces_indices <- seq.int(3L, length(face_vertex_indices), by = 3L)
  face_vertex_indices[last_vertices_of_faces_indices] <- -face_vertex_indices[last_vertices_of_faces_indices]
  face_lines <- fixed.vec.format.int(face_vertex_indices, num_chars_per_entry = 6L, max_entries_per_line = 16L)
  writeLines(face_lines, fh)

  close(fh)
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
write.fs.surface.bvsrf <- function(filepath, vertex_coords, faces, normals = NULL, neighborhoods = NULL) {
  num_verts <- nrow(vertex_coords)
  num_faces <- nrow(faces)
  endian <- "little"

  check.verts.faces(vertex_coords, faces)

  if (is.null(normals)) {
    normals <- rep(0.0, (num_verts * 3L))
    normals <- matrix(normals, ncol = 3)
  }

  fh <- file(filepath, "wb")
  writeBin(as.double(4.0), fh, size = 4, endian = endian) # SRF file format version
  writeBin(as.integer(0L), fh, size = 4, endian = endian) # must be 0
  writeBin(as.integer(num_verts), fh, size = 4, endian = endian)
  writeBin(as.integer(num_faces), fh, size = 4, endian = endian)

  writeBin(as.double(128.0), fh, size = 4, endian = endian) # next 3 are mesh center x,y,z
  writeBin(as.double(128.0), fh, size = 4, endian = endian)
  writeBin(as.double(128.0), fh, size = 4, endian = endian)

  writeBin(as.double(vertex_coords[, 1]), fh, size = 4, endian = endian) # vert coord x, z, y
  writeBin(as.double(vertex_coords[, 2]), fh, size = 4, endian = endian)
  writeBin(as.double(vertex_coords[, 3]), fh, size = 4, endian = endian)

  writeBin(as.double(normals[, 1]), fh, size = 4, endian = endian) # vert normals x, z, y
  writeBin(as.double(normals[, 2]), fh, size = 4, endian = endian)
  writeBin(as.double(normals[, 3]), fh, size = 4, endian = endian)

  writeBin(as.double(0.322), fh, size = 4, endian = endian) # convex vert color RGBA
  writeBin(as.double(0.733), fh, size = 4, endian = endian)
  writeBin(as.double(0.980), fh, size = 4, endian = endian)
  writeBin(as.double(1.000), fh, size = 4, endian = endian)

  writeBin(as.double(0.100), fh, size = 4, endian = endian) # concave vert color RGBA
  writeBin(as.double(0.240), fh, size = 4, endian = endian)
  writeBin(as.double(0.320), fh, size = 4, endian = endian)
  writeBin(as.double(1.000), fh, size = 4, endian = endian)

  mesh_col <- rep(0L, num_verts)
  writeBin(as.integer(mesh_col), fh, size = 4, endian = endian)

  # nearest neighbor data
  if (is.null(neighborhoods)) {
    neighborhood_sizes <- rep(0L, num_verts) # we do not have neighborhood data
    writeBin(as.integer(neighborhood_sizes), fh, size = 4, endian = endian)
  } else {
    for (neighbors in neighborhoods) {
      writeBin(as.integer(length(neighbors)), fh, size = 4, endian = endian)
      writeBin(as.integer(neighbors), fh, size = 4, endian = endian)
    }
  }

  # writes faces
  faces <- faces - 1L
  writeBin(as.integer(t(faces)), fh, size = 4, endian = endian)
  writeBin(as.integer(0L), fh, size = 4, endian = endian) # num triangle strips
  writeChar("", fh) # associated file name
  close(fh)
}

# lh = freesurferformats::read.fs.surface('~/data/tim_only/tim/surf/lh.white')
# cm = fsbrain::coloredmesh.from.morph.native('~/data/tim_only', 'tim', 'sulc', hemi='lh')
# freesurferformats::write.fs.surface.obj("~/lh.obj", lh$vertices, lh$faces, vertex_colors=cm$col)
# library('rayrender')
# bscene = generate_ground() %>% add_object(rayrender::obj_model("~/lh.obj", x= 20, y = 200, z = 20, material = diffuse(sigma=90), vertex_colors = TRUE))
# render_scene(bscene, parallel = TRUE, width = 800, height = 800, samples = 1000, lookfrom = c(-550, 160, 0), lookat = c(0, 180, 80))
