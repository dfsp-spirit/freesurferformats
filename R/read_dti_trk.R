# Functions to read DTI fiber track data in the '.trk' format used by the Diffusion Toolkit and TrackVis.
# See http://trackvis.org/docs/?subsect=fileformat for the spec.
# For some demo files in trk format, check nibabel.

#' @title Read fiber tracks from Diffusion Toolkit in trk format.
#'
#' @param filepath character string, path to file in trk format.
#'
#' @param shift_origin logical, whether to apply the half-voxel origin shift when computing the corrected vox2ras matrix. The TRK format stores a matrix that maps to the voxel corner, not the voxel center (as is the NIfTI convention). Set to `TRUE` (the default) to compute the corrected `vox2ras` that maps to voxel centers, as used by TrackVis. Set to `FALSE` if the file was written by DSI Studio, which does not apply this shift. See the notes for details.
#'
#' @return named list, the parsed file data. The naming of the variables follows the spec at \code{http://trackvis.org/docs/?subsect=fileformat}. The returned header will contain the field `vox2ras` (the raw matrix stored in the TRK file, mapping from mm space to RAS) and, if `shift_origin` is `TRUE`, the additional field `vox2ras_corrected` (the computed matrix mapping from voxel indices to voxel center RAS coordinates).
#'
#' @note The 4x4 matrix stored in TRK files (labeled `vox_to_ras` in the spec) is actually a transformation from **mm** space to RAS, not from voxel space to RAS. The TRK format was designed by TrackVis with the assumption that voxels are 1 mm\eqn{^3}, and that coordinates refer to voxel corners rather than centers. To obtain the actual vox2ras matrix (voxel center in RAS), the raw matrix must be combined with a voxel-size scaling and a half-voxel offset correction: `vox2ras_corrected = mm2ras %*% mm_correction %*% vox2mm`, where `vox2mm` scales by the inverse voxel size and `mm_correction` shifts by -0.5 mm. Note that DSI Studio does **not** apply this half-voxel shift, so you may need to set `shift_origin=FALSE` for DSI Studio files.
#'
#' @examples
#' \dontrun{
#' trk = read.dti.trk("~/simple.trk");
#' trk2 = read.dti.trk("~/standard.trk");
#' trk3 = read.dti.trk("~/complex_big_endian.trk");
#' }
#'
#' @export
read.dti.trk <- function(filepath, shift_origin = TRUE) {
  endian = get.dti.trk.endianness(filepath);

  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  trk = list('header' = list());

  trk$header$id_string = read.fixed.char.binary(fh, 6L);
  trk$header$dim = readBin(fh, integer(), n = 3, size = 2, endian = endian);
  trk$header$voxel_size = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk$header$origin = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk$header$n_scalars = readBin(fh, integer(), n = 1, size = 2, endian = endian); # scalar: one value per point (on a track)
  trk$header$scalar_names = read.fixed.char.binary(fh, 200L);
  trk$header$n_properties = readBin(fh, integer(), n = 1, size = 2, endian = endian); # property: one value per track.
  trk$header$property_names = read.fixed.char.binary(fh, 200L);
  trk$header$vox2ras = matrix(readBin(fh, numeric(), n = 16, size = 4, endian = endian), ncol = 4, byrow = TRUE);
  if(shift_origin) {
    vox2mm = diag(c(1.0 / trk$header$voxel_size, 1.0), nrow = 4L);
    mm_correction = diag(1.0, nrow = 4L);
    mm_correction[1:3, 4] = -0.5;
    trk$header$vox2ras_corrected = trk$header$vox2ras %*% mm_correction %*% vox2mm;
  }
  trk$header$reserved = read.fixed.char.binary(fh, 444L);
  trk$header$voxel_order = read.fixed.char.binary(fh, 4L);
  trk$header$pad2 = read.fixed.char.binary(fh, 4L); # padding
  trk$header$image_orientation_patient = readBin(fh, numeric(), n = 6, size = 4, endian = endian);
  trk$header$pad1 = read.fixed.char.binary(fh, 2L); # padding
  trk$header$invert_x = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$invert_y = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$invert_z = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$swap_xy = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$swap_yz = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$swap_zx = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk$header$n_count = readBin(fh, integer(), n = 1, size = 4, endian = endian); # number of tracks, 0=not stored/unknown.
  trk$header$version = readBin(fh, integer(), n = 1, size = 4, endian = endian); # file format version
  trk$header$hdr_size = readBin(fh, integer(), n = 1, size = 4, endian = endian); # size of hdr, for endianess checking.

  if(trk$header$version != 2L) {
    warning(sprintf("TRK file '%s' has version %d, only version 2 is supported.\n", filepath, trk$header$version));
  }
  if(trk$header$hdr_size != 1000L) {
    warning(sprintf("TRK file '%s' header field hdr_size is '%d', must be 1000.\n", filepath, trk$header$hdr_size));
  }

  tracks = list();

  # Read TRACK data
  if(trk$header$n_count > 0L) {
    for(track_idx in 1L:trk$header$n_count) {
      current_track = list('scalars' = NULL, 'properties' = NULL, 'coords' = NULL);
      current_track$num_points = readBin(fh, integer(), n = 1, size = 4, endian = endian);
      current_track$coords = matrix(rep(NA, (current_track$num_points * 3L)), ncol = 3);

      if(trk$header$n_scalars > 0L) {
        current_track$scalars = matrix(rep(NA, (current_track$num_points * trk$header$n_scalars)), ncol = trk$header$n_scalars);
      }

      if(current_track$num_points > 0L) {
        for(track_point_idx in 1L:current_track$num_points) {
          current_track$coords[track_point_idx, ] = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
          if(trk$header$n_scalars > 0L) {
            current_track$scalars[track_point_idx, ] = readBin(fh, numeric(), n = trk$header$n_scalars, size = 4, endian = endian);
          }
        }
      }

      if(trk$header$n_properties > 0L) {
        current_track$properties = readBin(fh, numeric(), n = trk$header$n_properties, size = 4, endian = endian);
      }

      tracks[[track_idx]] = current_track;
    }
  }

  trk$tracks = tracks;
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
