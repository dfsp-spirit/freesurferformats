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
#' @return character string, the path to the first existing file (or NULL if none of them exists).
#'
#' @keywords internal
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
      this_file_ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1, nchar(filepath));
      if(tolower(this_file_ext) == tolower(ext)) {
        return(TRUE);
      }
    }
  }
  return(FALSE);
}
