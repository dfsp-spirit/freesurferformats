#' @title Find files with the given base name and extensions that exist.
#'
#' @description Note that in the current implementation, the case of the filepath and the extension must match.
#'
#' @param filepath character string, path to a file without extension
#'
#' @param precedence vector of character strings, the file extensions to check. Must include the dot (if you expect one).
#'
#' @param error_if_none logical, whether to raise an error if none of the files exist
#'
#' @param return_all logical, whether to return all readable files instead of just the first one
#'
#' @return character string, the path to the first existing file (or `NULL` if none of them exists).
#'
#' @export
readable.files <- function(filepath, precedence=c('.mgh', '.mgz'), error_if_none=TRUE, return_all=FALSE) {
  candidate_files = paste(filepath, precedence, sep='');
  readable_files = c();
  for(cfile in candidate_files) {
    if(file.exists(cfile)) {
      if(return_all) {
        readable_files = c(readable_files, cfile);
      } else {
        return(cfile);
      }
    }
  }

  if(length(readable_files) == 0 & error_if_none) {
    stop(sprintf("At location '%s' exists no file with any of the %d extensions '%s'.\n", filepath, length(precedence), paste(precedence, collapse=' ')));
  } else {
    return(readable_files);
  }
}


#' @title Check whether filepath ends with extension.
#'
#' @param filepath string. Path to a file, including filename and extension.
#'
#' @param extensions list of strings. A list of suffixes to check. Case does not matter. Example: \code{extensions=c('.gz', '.mgz')}.
#'
#' @return logical, whether the filepath end with one of the extensions.
#'
#' @keywords internal
filepath.ends.with <- function(filepath, extensions) {
  nc = nchar(filepath);
  for (ext in extensions) {
    num_chars_to_inspect = nchar(ext);
    if(nc >= num_chars_to_inspect) {
      this_file_ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1L, nchar(filepath));
      if(tolower(this_file_ext) == tolower(ext)) {
        return(TRUE);
      }
    }
  }
  return(FALSE);
}


#' @title Get an rgl tmesh3d instance from a brain surface mesh.
#'
#' @description Convert \code{fs.surface} to \code{tmesh} without the \code{rgl} package.
#'
#' @param surface an fs.surface instance, as returned \code{freesurferformats::read.fs.surface}.
#'
#' @return a \code{tmesh3d} instance representing the surface, see \code{rgl::tmesh3d} for details. It has classes \code{mesh3d} and \code{shape3d}.
#'
#' @export
fs.surface.to.tmesh3d <- function(surface) {
  if( ! is.fs.surface(surface)) {
    stop("Parameter 'surface' must be an instance of fs.surface.");
  }
  tmesh = list("material"=list(), "normals"=NULL, "texcoords"=NULL, "meshColor"="vertices");
  class(tmesh) = c("mesh3d", "shape3d");
  tmesh$vb = t(cbind(surface$vertices, 1L)); # Transform vertex coords to homogeneous and swap rows/columns
  tmesh$it = t(surface$faces); # swap only
  return(tmesh);
}


#' @title Check for pandoc availability on system.
#'
#' @return logical, whether Pandoc is available.
#'
#' @keywords internal
has_pandoc <- function() {
  assert_package("rmarkdown")
  return(rmarkdown::pandoc_available());
}


assert_package <- function(pkg) {

  package_installed <- vapply(pkg, function(p) {
    return( system.file(package = p) != "" )
  }, FALSE)
  if( all(package_installed) ) { return(invisible()) }
  pkg <- pkg[!package_installed]

  # the package is missing
  cnd <- structure(list(message = sprintf("Package(s) %s missing. Please install first.", paste(sQuote(pkg), collapse = ", ")), call = NULL),
                   class = c("package_not_found_error", "simpleError", "error", "condition"))

  # check if rlang has been installed, usually yes if people installs any rlib/posit packages
  rlang_is_missing <- system.file(package = "rlang") == ""
  pak_is_missing <- system.file(package = "pak") == ""

  # if not interactive or rlang&pak are missing, per CRAN policy, stop
  if(!interactive() || (rlang_is_missing && pak_is_missing)) {
    stop(cnd)
  }

  if( !rlang_is_missing ) {
    rlang <- asNamespace("rlang")
    rlang$check_installed(pkg)
    return(invisible())
  }

  # using pak
  message(sprintf("Package(s) %s missing, install?", paste(sQuote(pkg), collapse = ", ")))
  if (utils::menu(c("Yes", "No")) != 1) {
    invokeRestart("abort", cnd)
  }
  pak <- asNamespace("pak")
  pak$pkg_install(pkg, ask = FALSE)

  return(invisible())

}

