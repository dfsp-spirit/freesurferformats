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


#' @title Read DTI tracking per-coord data from file in MRtrix 'TSF' format.
#'
#' @param filepath character string, path to the \code{TSF} file to read.
#'
#' @note The data in such a file is one value per track point, the tracks are not part of the file but come in the matching TCK file.
#'
#' @seealso \code{read.dti.tck}
#'
#' @examples
#' \dontrun{
#'  tsff = "~/simple.tsf";
#'  tsf = read.dti.tsf(tsff);
#' }
#'
#' @return named list with entries 'header' and 'scalars'. The scala data are available in 2 representations: 'merged': a vector of all values (requires external knowledge on track borders), and 'scalar_list': organized into a list of vectors. Each vector represents the values for the points of one track.
#'
#' @export
read.dti.tsf <- function(filepath) {

  tsf = list('header' = list('derived' = list()), 'scalars' = list());

  all_lines = suppressWarnings(readLines(filepath));
  if(length(all_lines) < 4L) {
    stop("File not in TSF format: too few lines.");
  }

  tsf$header$id = all_lines[1];
  if(tsf$header$id != "mrtrix track scalars") {
    stop("File not in TSF format: Invalid first line.");
  }

  for(line_idx in 2L:length(all_lines)) {
    current_line = all_lines[line_idx];
    if(current_line == "END") {
      break;
    } else {
      line_parts = unlist(strsplit(current_line, ':'));
      lkey = trimws(line_parts[1]);
      lvalue = trimws(line_parts[2]);
      tsf$header[[lkey]] = lvalue;
      if(lkey == "file") {
        file_parts = unlist(strsplit(lvalue, ' '));
        tsf$header$derived$filename_part = trimws(file_parts[1]); # for future multi-file support, currently always '.'
        if(tsf$header$derived$filename_part != ".") {
          stop("Multi-file TSF files not supported.");
        }
        tsf$header$derived$data_offset = as.integer(trimws(file_parts[2]));
      }
    }
  }


  all_lines = NULL; # free, no longer needed.
  valid_datatypes = c('Float32BE', 'Float32LE', 'Float64BE', 'Float64LE');
  if(! tsf$header$datatype %in% valid_datatypes) {
    stop("Invalid datatype in TSF file header");
  }

  if(is.null(tsf$header$derived$data_offset)) {
    stop("Invalid TSF file, missing file offset header entry.");
  }

  # Determine endianness of following binary data.
  tsf$header$derived$endian = "little";
  if(endsWith(tsf$header$datatype, 'BE')) {
    tsf$header$derived$endian = "big";
  }

  # Determine size of entries in bytes.
  tsf$header$derived$dsize = 4L; # default to 32bit
  if(startsWith(tsf$header$datatype, 'Float64')) {
    tsf$header$derived$dsize = 8L;
  }

  # Read binary scalar data.
  fs = file.size(filepath);
  num_to_read = (fs - tsf$header$derived$data_offset) / tsf$header$derived$dsize;

  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  seek(fh, where = tsf$header$derived$data_offset, origin = "start");
  scalar_rawdata = readBin(fh, numeric(), n = num_to_read, size = tsf$header$derived$dsize, endian = tsf$header$derived$endian);
  # NaN and Inf values are track separators.

  # Generate single vector representation. The user will have to split by tracks based on
  # knowledge about track borders from the TCK file.
  data_indices = which(!(is.nan(scalar_rawdata) | is.infinite(scalar_rawdata)));
  tsf$scalars$merged = scalar_rawdata[data_indices];

  # Generate the alternative list representation of the scalar data:
  # Filter separators and end marker, organize into tracks list (of matrices).
  tsf$scalars$scalar_list = list();
  current_track_idx = 1L;
  for(value_idx in 1L:length(scalar_rawdata)) {
    current_value = scalar_rawdata[value_idx];
    if(is.nan(current_value) | is.infinite(current_value)) {
      current_track_idx = current_track_idx + 1L;
      next;
    } else {
      if(length(tsf$scalars$scalar_list) < current_track_idx) {
        tsf$scalars$scalar_list[[current_track_idx]] = current_value;
      } else {
        tsf$scalars$scalar_list[[current_track_idx]] = c(tsf$scalars$scalar_list[[current_track_idx]], current_value);
      }
    }
  }

  return(tsf);
}
