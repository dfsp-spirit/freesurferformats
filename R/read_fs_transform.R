#' @title  Load transformation matrix from a file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @param format character string, the file format. Currently 'xfm' or 'dat' (for tkregister style, e.g. register.dat) are supported.
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @note Currently this function has been tested with linear transformation files only, all others are unsupported.
#'
#' @family header coordinate space
#'
#' @examples
#'     tf_file = system.file("extdata", "talairach.xfm",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     transform = read.fs.transform(tf_file);
#'     transform$matrix;
#'
#' @export
read.fs.transform <- function(filepath, format='xfm') {

  if( ! format %in% c('xfm', 'dat')) {
    stop("Format must be 'xfm'.");
  }

  if(format == "xfm" || (format == 'auto' & endsWith(filepath, '.xfm') )) {
    return(read.fs.transform.xfm(filepath));
  }
  if(format == "dat" || (format == 'auto' & endsWith(filepath, '.dat') )) {
    return(read.fs.transform.dat(filepath));
  }
}


#' @title  Load transformation matrix from an XFM file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @note Currently this function has been tested with linear transformation files only, all others are unsupported.
#'
#' @family header coordinate space
#'
#' @examples
#'     tf_file = system.file("extdata", "talairach.xfm",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     transform = read.fs.transform.xfm(tf_file);
#'     transform$matrix;
#'
#' @export
read.fs.transform.xfm <- function(filepath) {

  transform = list('type'=NULL, 'matrix'=NULL);

  all_lines = readLines(filepath);
  current_line_idx = 1L;
  while(current_line_idx <= length(all_lines)) {
    current_line = all_lines[current_line_idx];
    if(startsWith(current_line, 'Transform_Type')) {
      transform_type_definition_parts = strsplit(current_line, "=")[[1]];
      transform_type_definition_parts_trimmed = trimws(transform_type_definition_parts);
      transform$type = trimws(transform_type_definition_parts_trimmed[2], which = "right", whitespace = "[;]");
    }

    if(endsWith(trimws(current_line), 'Transform =')) {
      if(length(all_lines) < current_line_idx+3L) {
        stop(sprintf("Expected transformation matrix in lines %d-%d, but file only has %d lines.\n", current_line_idx+1L, current_line_idx+3L, length(all_lines)));
      }
      idx_matrix_start = current_line_idx + 1L;
      idx_matrix_end = idx_matrix_start + 2L;
      matrix_lines = all_lines[idx_matrix_start:idx_matrix_end];
      transform$matrix = parse.transform.matrix.lines(matrix_lines);
    }

    current_line_idx = current_line_idx + 1L;
  }
  if(is.null(transform$type) | is.null(transform$matrix)) {
    if(is.null(transform$type)) {
      stop("Invalid xfm file, did not find required transform type.");
    }
    stop("Invalid xfm file, did not find required transform matrix.");
  }
  return(transform);
}


#' @title  Load transformation matrix from a tkregister dat file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return 4x4 numerical matrix, the transformation matrix
#'
#' @family header coordinate space
#'
#' @examples
#'     tf_file = system.file("extdata", "register.dat",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     transform = read.fs.transform.dat(tf_file);
#'     transform$matrix;
#'
#' @export
read.fs.transform.dat <- function(filepath) {

  transform = list('type'=NULL, 'matrix'=NULL);

  all_lines = readLines(filepath);
  if(length(all_lines) != 9L) {
    stop(sprintf("Expected 9 lines in tkregister dat file, found %d.\n", length(all_lines)));
  }

  transform$intensity = as.integer(trimws(all_lines[4]));
  transform$matrix = parse.transform.matrix.lines(all_lines[5:8]);

  return(transform);
}



#' @title Parse matrix from text file lines.
#'
#' @param file_lines vector of 3 character strings, the matrix lines. The separator is assumed to be a single space.
#'
#' @param ignore_line_suffix character string, a line suffix that will be stripped from the end of each line if it exists.
#'
#' @return numerical 4x4 matrix, the parsed matrix
#'
#' @keywords internal
parse.transform.matrix.lines <- function(file_lines, ignore_line_suffix=";") {

  tm = matrix(rep(0., 16L), ncol = 4L);
  tm[4, ] = c(0, 0, 0, 1); # Will be overwritten below if 4 file_lines are given.

  if(! length(file_lines) %in% c(3L, 4L)) {
    stop(sprintf("Parameter 'lines' must be of length 3 or 4, is %d.\n", length(file_lines)));
  }

  line_idx = 1L;
  for(file_line in file_lines) {
    if(endsWith(file_line, ignore_line_suffix)) {
      file_line = substring(file_line, 1L, (nchar(file_line) - nchar(ignore_line_suffix)));
    }
    matrix_row = as.double(strsplit(file_line, " ")[[1]]);
    tm[line_idx, ] = matrix_row;
    line_idx = line_idx + 1L;
  }
  return(tm);
}
