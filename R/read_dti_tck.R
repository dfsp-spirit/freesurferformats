# Functions for reading DTI tracking data from files in MRtrix TCK format.
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
#' @return named list with entries 'header' and 'tracks'. The tracks are organized into a list of matrices. Each n x 3 matrix represents the coordinates for the n points of one track, the values in each row are the xyz coords.
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
  valid_datatypes = c('Float32BE', 'Float32LE', 'Float64BE', 'Float64LE');
  if(! tck$header$datatype %in% valid_datatypes) {
    stop("Invalid datatype in TCK file header");
  }

  if(is.null(tck$header$derived$data_offset)) {
    stop("Invalid TCK file, missing file offset header entry.");
  }

  # Determine endianness of following binary data.
  tck$header$derived$endian = "little";
  if(endsWith(tck$header$datatype, 'BE')) {
    tck$header$derived$endian = "big";
  }

  # Determine size of entries in bytes.
  tck$header$derived$dsize = 4L; # default to 32bit
  if(startsWith(tck$header$datatype, 'Float64')) {
    tck$header$derived$dsize = 8L;
  }

  # Read binary track data.
  fs = file.size(filepath);
  num_to_read = (fs - tck$header$derived$data_offset) / tck$header$derived$dsize;

  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  seek(fh, where = tck$header$derived$data_offset, origin = "start");
  tracks_rawdata = readBin(fh, numeric(), n = num_to_read, size = tck$header$derived$dsize, endian = tck$header$derived$endian);

  # Rows consisting of NaNs are track separators, and the final EOF row is all Inf.
  tracks_raw_matrix = matrix(tracks_rawdata, ncol = 3, byrow = TRUE);

  # Filter separators and end marker, organize into tracks list (of matrices).
  tck$tracks = list();
  current_track_idx = 1L;
  for(row_idx in 1L:nrow(tracks_raw_matrix)) {
    if(any(is.nan(tracks_raw_matrix[row_idx, ])) | any(is.infinite(tracks_raw_matrix[row_idx, ]))) {
      current_track_idx = current_track_idx + 1L;
      next;
    } else {
      if(length(tck$tracks) < current_track_idx) {
        tck$tracks[[current_track_idx]] = tracks_raw_matrix[row_idx, ];
      } else {
        tck$tracks[[current_track_idx]] = rbind(tck$tracks[[current_track_idx]], tracks_raw_matrix[row_idx, ]);
      }
    }
  }

  return(tck);
}

