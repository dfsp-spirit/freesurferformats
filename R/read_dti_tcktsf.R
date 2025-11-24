# Function for reading DTI tracking data from files in MRtrix TCK/TSF --------

#' .read.dti.tcktsf
#' @keywords internal
.read.dti.tcktsf <- function(filepath) {
  # https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html#tracks-file-format-tck
  # https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html#track-scalar-file-format-tsf
  L = readLines(filepath, n=50L, warn=FALSE)

  id = trimws(L[1])
  if (!id %in% c('mrtrix tracks', 'mrtrix track scalars'))
    stop('File not in TCK/TSF format: invalid first line.')

  fs = file.size(filepath)
  if (fs/1024 < 100)
    stop('File not in TCK/TSF format: file too small.')

  k = grep(pattern='END', x=L, useBytes=TRUE)
  if (length(k)==0)
    stop('File not in TCK/TSF format: invalid format.')

  header = read.table(text=L[2:(k-1)], sep=':', header=FALSE)
  header = c(list(id=id), utils::type.convert(split.default(trimws(header$V2), header$V1), as.is=TRUE))

  if (!grepl(pattern='.', x=header$file))
    #  "only the single-file format is supported [...] file name part must be specified as '.'"
    stop('Multi-file TCK/TSF files not supported.') else {
      tmp = strsplit(header$file, ' ', fixed=TRUE)
      filename_part = tmp[[1]][1]
      offset = strtoi(tmp[[1]][2])
    }

  if (strtoi(header$count)==0L) # system('tckinfo <path> -count')
    stop('Invalid datatype. Number of streamlines ("count") is zero.')

  valid_datatypes = c('Float32BE', 'Float32LE', 'Float64BE', 'Float64LE')
  if (!header$datatype %in% valid_datatypes)
    stop('Invalid data type in TCK/TSF file header.')

  endian = if (endsWith(header$datatype, 'BE')) 'big' else 'little'
  dsize = if (startsWith(header$datatype, 'Float64')) 8L else 4L
  n2r = (fs - offset) / dsize
  derived = list(derived = list(filename_part=filename_part, data_offset=offset, endian=endian, dsize=dsize))

  fh = file(description=filepath, open='rb')
  on.exit(close(fh), add=TRUE)
  seek(con=fh, where=offset, origin='start')
  rawdata = readBin(con=fh, what=numeric(), n=n2r, size=dsize, endian=endian)

  if (id=='mrtrix tracks') { # TCK
    # "The binary track data themselves are stored as triplets of floating-point values:"
    tracks_raw_matrix = matrix(rawdata, ncol=3, byrow=TRUE)
    # "tracks are separated using a triplet of NaN (Not A Number) values" and
    # "a triplet of Inf values is used to indicate the end of the file"
    i = rowSums(is.finite(tracks_raw_matrix))==3
    tracks = split.data.frame(tracks_raw_matrix[i, ], cumsum(!i)[i]) |> unname()

    list(header=c(derived, header), tracks=tracks)

    } else { # TSF
    # "a .tsf files contains one floating-point value per streamline vertex,
    # with one NaN value delimiting between streamlines and one Inf value
    # indicating the end of the file."
    i = is.finite(rawdata)
    scalars = rawdata[i]
    scalar_list = split(scalars, cumsum(!i)[i]) |> unname()

    list(header=c(derived, header), scalars=list(merged=scalars, scalar_list=scalar_list))
  }
}


# Exported functions ------------------------------------------------------

#' @title Read DTI tracking data from file in MRtrix 'TCK' format.
#'
#' @param filepath character string, path to the \code{TCK} file to read.
#'
#' @examples
#' \dontrun{
#'  tckf = "~/simple.tck"
#'  tck = read.dti.tck(tckf)
#' }
#'
#' @return named list with entries 'header' and 'tracks'. The tracks are organized into a list of matrices. Each n x 3 matrix represents the coordinates for the n points of one track, the values in each row are the xyz-coords.
#'
#' @export
read.dti.tck <- function(filepath) {
  .read.dti.tcktsf(filepath)
}


#' @title Read DTI tracking per-coord data from file in MRtrix 'TSF' format.
#'
#' @param filepath character string, path to the \code{TSF} file to read.
#'
#' @examples
#' \dontrun{
#'  tsff = "~/simple.tsf"
#'  tsf = read.dti.tsf(tsff)
#' }
#'
#' @return named list with entries 'header' and 'scalars'. The scalar data are available in 2 representations: 'merged': a vector of all values (requires external knowledge on track borders), and 'scalar_list': organized into a list of vectors. Each vector represents the values for the points of one track.
#'
#' @export
read.dti.tsf <- function(filepath) {
  .read.dti.tcktsf(filepath)
}
