# Functions for reading DTI tracking data from filrs in TCK format.
# See http://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html?highlight=format#tracks-file-format-tck for a spec.


#' @title Read DTI tracking data from file in MRtrix 'TCK' format.
#'
#' @param filepath character string, path to the \code{TCK} file to read.
#'
#' @examples
#' \dontrun{
#'  tckf = "~/simple.tck";
#'  tck = read.dti.tck(tckf);
#' }
#'
#' @export
read.dti.tck <- function(filepath) {

  tck = list('header' = list('derived' = list()));

  all_lines = suppressWarnings(readLines(filepath));
  if(length(all_lines) < 4L) {
    stop("File not in TCK format: too few lines.");
  }

  tck$header$id = all_lines[1];
  if(tck$header$id != "mrtrix tracks") {
    stop("File not in TCK format: Invalid first line.");
  }

  for(line_idx in 2L:length(all_lines)) {
    current_line = all_lines[line_idx];
    if(current_line == "END") {
      break;
    } else {
      line_parts = unlist(strsplit(current_line, ':'));
      lkey = trimws(line_parts[1]);
      lvalue = trimws(line_parts[2]);
      tck$header[[lkey]] = lvalue;
      if(lkey == "file") {
        file_parts = unlist(strsplit(lvalue, ' '));
        tck$header$derived$filename_part = trimws(file_parts[1]); # for future multi-file support, currently always '.'
        if(tck$header$derived$filename_part != ".") {
          stop("Multi-file TCK files not supported.");
        }
        tck$header$derived$data_offset = as.integer(trimws(file_parts[2]));
      }
    }
  }

  all_lines = NULL; # free, no longer needed.

  # TODO: read binary track data at offset.
  #tck$tracks =

  return(tck);
}

