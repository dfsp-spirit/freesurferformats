# Functions to read different neuroimaging surface files formats.
# Uses S3 generics to try several loading methods (file formats) for the same file.


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
#' \dontrun{
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
#' @description Tries to read the file with all implemented surface format reader methods. The file must exist. With the default settings, one can read files in the following surface formats: 1) FreeSurfer binary surface format (e.g., `surf/lh.white`). 2) FreeSurfer ASCII surface format (e.g., `surf/lh.white,asc`). 3) GIFTI surface format, only if package `gifti` is installed. See \code{gifti::read_gifti} for details. Feel free to implement additional methods. Hint:keep in mind that they should return one-based indices.
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
    if(is.null(res$data$pointset) | is.null(res$data$triangle)) {
      # This is a GIFTI file, but it does not contain a mesh. Note that GIFTI can contain various data types. Maybe the user accidently passed a func GIFTI file.
      NextMethod('read_nisurfacefile');
    } else {
      ret_list = list("vertices"=res$data$pointset, "faces"=matrix(as.integer(res$data$triangle + 1L), ncol=3L), "mesh_face_type" = 'tris');
      class(ret_list) = c("fs.surface", class(ret_list));
      return(ret_list);
    }
  }

  # Failed, use the next read.ni.surface.* method
  NextMethod('read_nisurfacefile');
}

#' @export
read_nisurfacefile.default <- function(filepath, ...) {
  stop(sprintf("Surface file '%s' could not be read with any of the available methods, format invalid or not supported.\n", filepath));
}

