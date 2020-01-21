#' @title Write a surface patch
#'
#' @description Write a surface patch, i.e. a set of vertices and patch border information, to a binary patch file.
#'
#' @param filepath string. Full path to the output patch file. If it ends with ".gz", the file is written in gzipped format. Note that this is not common, and that other software may not handle this transparently.
#'
#' @param patch an instance of class `fs.patch`, see
#'
#' @return the patch, invisible
#'
#' @family patch functions
#' @export
write.fs.patch <- function(filepath, patch) {

  if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  # write file version code
  writeBin(as.integer(-1L), fh, size = 4, endian = "big");

  num_verts = nrow(patch$vertices);
  writeBin(as.integer(num_verts), fh, size = 4, endian = "big"); # write vertex count

  for(row_idx in seq_len(num_verts)) {
    vtx_raw = as.integer(patch$vertices[row_idx, 6]);
    writeBin(as.integer(vtx_raw), fh, size = 4, endian = "big");
    vertex_coords = as.double(patch$vertices[row_idx, 2:4]);
    writeBin(vertex_coords, fh, size = 4, endian = "big");
  }

  close(fh);

  return(invisible(patch));
}


#' @title Constructor for fs.patch
#'
#' @param vertices numerical *n*x5 matrix (or *n*x7 matrix), see \code{\link[freesurferformats]{read.fs.patch}} for details. If it has 5 columns, columns 6-7 will be computed automatically from the first 5 columns (from column 1 and 5).
#'
#' @param faces numerical *n*x5 matrix, see \code{\link[freesurferformats]{read.fs.patch.asc}} for details. Can be `NULL`.
#'
#' @return instance of class `fs.patch`
#'
#' @examples
#'     num_vertices = 6L;   # tiny patch
#'     vertices = matrix(rep(0., num_vertices*5), ncol=5);
#'     vertices[,1] = seq.int(num_vertices);  # 1-based vertex indices
#'     vertices[,2:4] = matrix(rnorm(num_vertices*3, 8, 2), ncol=3);  # vertex coords
#'     vertices[,5] = rep(0L, num_vertices);  # is_border
#'     vertices[3,5] = 1L;  # set a vertex to be a border vertex
#'     patch = fs.patch(vertices);
#'
#' @export
fs.patch <- function(vertices, faces=NULL) {
  ret_list = list();

  if(ncol(vertices) != 7L) {
    if(ncol(vertices) == 5L) {
      # infer the other columns
      vert_index1 = as.integer(vertices[,1]);
      is_border = as.logical(vertices[,5]);
      raw_vtx = ifelse(is_border, -vert_index1, vert_index1);
      vertices = cbind(vertices, raw_vtx);
      vertices = cbind(vertices, vert_index1 - 1L);
    } else {
      stop("Parameter 'vertices' must be a matrix with 5 or 7 columns.");
    }
  }
  colnames(vertices) = c("vert_index1", "x", "y", "z", "is_border", "vtx_raw", "vert_index0");

  if(!is.null(faces)) {
    if(ncol(faces) != 5L) {
      stop("Parameter 'faces' must be a matrix with 5 columns unless it is NULL.");
    }
  }

  ret_list$vertices = vertices;
  ret_list$faces = faces;

  class(ret_list) = c("fs.patch", class(ret_list));
  return(ret_list);
}


#' @title Print description of a brain surface patch.
#'
#' @param x brain surface patch with class `fs.patch`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.patch <- function(x, ...) {
  cat(sprintf("Brain surface patch with %d vertices, %d on patch border.\n", nrow(x$vertices), sum(as.integer(patch$vertices[,5]))));
  if(is.null(x$faces)) {
    cat(sprintf("Patch contains no face information.\n"));
  } else {
    cat(sprintf("Patch contains %d faces.\n", nrow(x$faces)));
  }
}
