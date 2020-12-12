# Functions to read DTI fiber track data in the '.trk' format used by the Diffusion Toolkit and TrackVis.
# See http://trackvis.org/docs/?subsect=fileformat for the spec.
# For some demo files in trk format, check nibabel.

#' @title Read fiber tracks from Diffusion Toolkit in trk format.
#'
#' @param filepath character string, path to file in trk format.
#'
#' @return named list, the parsed file data.
#'
#' @examples
#' \dontrun{
#' trk = read.dti.trk("~/simple.trk");
#' }
#'
#' @export
read.dti.trk <- function(filepath) {
  endian = get.dti.trk.endianness(filepath);

  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  trk = list('header' = list());

  trk$header$id_string = read.fixed.char.binary(fh, 6L);
  trk$header$dim = readBin(fh, integer(), n = 3, size = 2, endian = endian);
  trk$header$voxel_size = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk$header$origin = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk$header$n_scalars = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  trk$header$scalar_names = read.fixed.char.binary(fh, 200L);
  trk$header$n_properties = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  trk$header$property_names = read.fixed.char.binary(fh, 200L);
  trk$header$vox2ras = matrix(readBin(fh, numeric(), n = 16, size = 4, endian = endian), ncol = 4, byrow = TRUE);
  trk$header$reserved = read.fixed.char.binary(fh, 400L);
  trk$header$voxel_order = read.fixed.char.binary(fh, 4L);


  return(trk);
}


#' @title Determine endianness of TRK file.
#'
#' @inheritParams read.dti.trk
#'
#' @return endina character string. one of 'little' or 'big'.
#'
#' @note This function checks endiannes via the header size field of the file header, which must be 1000 for TRK files when read with correct enianness. It will stop if the file is not in TRK format, i.e., if the field is not 1000 in any endianness.
#'
#' @keywords internal
get.dti.trk.endianness <- function(filepath) {

  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  seek(fh, where = 996L, origin = "start");

  endian = 'little';
  sizeof_hdr_little = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if(sizeof_hdr_little == 1000L) {
    return(endian);
  } else {
    seek(fh, where = 996L, origin = "start");
    endian = 'big';
    sizeof_hdr_big = readBin(fh, integer(), n = 1, size = 4, endian = endian);
    if(sizeof_hdr_big == 1000L) {
      return(endian);
    } else {
      stop(sprintf("File '%s' not in TRK format (header sizes %d/%d in little/big endian mode, expected 1000).\n", filepath, sizeof_hdr_little, sizeof_hdr_big));
    }
  }
}