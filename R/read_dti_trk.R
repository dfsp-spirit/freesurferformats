# Functions to read DTI fiber track data in the '.trk' format used by the Diffusion Toolkit and TrackVis.
# See http://trackvis.org/docs/?subsect=fileformat for the spec.
# For some demo files in trk format, check nibabel.
#
# Note on the buffers in read.trk.records(): they are grown and filled with
# inline `data[from:to, ] <- record` assignments on purpose. Handing a buffer to
# a helper function and assigning the result back would give the object a second
# reference, and R would then copy the entire buffer on every single track,
# which makes reading quadratic in the number of tracks.


#' @title Read the header of a TrackVis TRK file.
#'
#' @description Reads and parses the 1000 byte header of a TRK file without
#'   touching the track data. This is cheap even for huge tractograms and can be
#'   used to inspect a file before deciding whether to read its tracks.
#'
#' @param filepath character string, path to a file in TRK format.
#'
#' @param shift_origin logical, whether to compute the corrected \code{vox2ras}
#'   matrix, see \code{\link{read.dti.trk}}.
#'
#' @return named list, the header. It contains all fields of the TRK header as
#'   described at \code{http://trackvis.org/docs/?subsect=fileformat}, plus
#'   \code{vox2ras} (the matrix stored in the file) and, if
#'   \code{shift_origin} is \code{TRUE}, \code{vox2ras_corrected}.
#'
#' @examples
#' \dontrun{
#' hdr <- read.dti.trk.header("brain.trk");
#' hdr$n_count;      # number of tracks stated in the header
#' hdr$voxel_order;
#' }
#'
#' @export
read.dti.trk.header <- function(filepath, shift_origin = TRUE) {
  endian <- get.dti.trk.endianness(filepath);

  fh <- file(filepath, "rb");
  on.exit(
    {
      close(fh);
    },
    add = TRUE
  );

  trk_header <- list();

  trk_header$id_string <- read.fixed.char.binary(fh, 6L);
  trk_header$dim <- readBin(fh, integer(), n = 3, size = 2, endian = endian);
  trk_header$voxel_size <- readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk_header$origin <- readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  trk_header$n_scalars <- readBin(fh, integer(), n = 1, size = 2, endian = endian); # scalar: one value per point (on a track)
  trk_header$scalar_names <- read.fixed.char.binary(fh, 200L);
  trk_header$n_properties <- readBin(fh, integer(), n = 1, size = 2, endian = endian); # property: one value per track.
  trk_header$property_names <- read.fixed.char.binary(fh, 200L);
  trk_header$vox2ras <- matrix(readBin(fh, numeric(), n = 16, size = 4, endian = endian), ncol = 4, byrow = TRUE);
  if (is.na(trk_header$vox2ras[4, 4]) || trk_header$vox2ras[4, 4] == 0) {
    # Per the TRK spec, a zero last diagonal entry means that the matrix was not
    # recorded. nibabel assumes the identity in that case as well.
    warning(sprintf("The 'vox_to_ras' matrix of TRK file '%s' was not recorded, assuming it is the identity.\n", filepath));
    trk_header$vox2ras <- diag(4L);
  }
  if (shift_origin) {
    vox2mm <- diag(c(1.0 / trk_header$voxel_size, 1.0), nrow = 4L);
    mm_correction <- diag(1.0, nrow = 4L);
    mm_correction[1:3, 4] <- -0.5;
    trk_header$vox2ras_corrected <- trk_header$vox2ras %*% mm_correction %*% vox2mm;
  }
  trk_header$reserved <- read.fixed.char.binary(fh, 444L);
  trk_header$voxel_order <- read.fixed.char.binary(fh, 4L);
  trk_header$pad2 <- read.fixed.char.binary(fh, 4L); # padding
  trk_header$image_orientation_patient <- readBin(fh, numeric(), n = 6, size = 4, endian = endian);
  trk_header$pad1 <- read.fixed.char.binary(fh, 2L); # padding
  trk_header$invert_x <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$invert_y <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$invert_z <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$swap_xy <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$swap_yz <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$swap_zx <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  trk_header$n_count <- readBin(fh, integer(), n = 1, size = 4, endian = endian); # number of tracks, 0=not stored/unknown.
  trk_header$version <- readBin(fh, integer(), n = 1, size = 4, endian = endian); # file format version
  trk_header$hdr_size <- readBin(fh, integer(), n = 1, size = 4, endian = endian); # size of hdr, for endianess checking.

  if (trk_header$version != 2L) {
    warning(sprintf("TRK file '%s' has version %d, only version 2 is supported.\n", filepath, trk_header$version));
  }
  if (trk_header$hdr_size != 1000L) {
    warning(sprintf("TRK file '%s' header field hdr_size is '%d', must be 1000.\n", filepath, trk_header$hdr_size));
  }

  return(trk_header);
}


#' @title Read the track records of a TRK file.
#'
#' @description Reads track records from the current position of the connection
#'   until the end of the file or until \code{max_tracks} records have been
#'   selected. Coordinates and per-point scalars are stored interleaved in the
#'   file, so each record is read with a single \code{readBin} call and split
#'   afterwards, instead of reading one point at a time.
#'
#' @param fh a connection opened in binary read mode, positioned at the start of
#'   the first track record.
#'
#' @param endian character string, 'little' or 'big'.
#'
#' @param n_scalars integer, number of scalars per point.
#'
#' @param n_properties integer, number of properties per track.
#'
#' @param max_tracks numeric, maximum number of tracks to select. Use
#'   \code{Inf} to read until the end of the file.
#'
#' @param expected_tracks numeric, the number of tracks stated in the header, or
#'   NA if the header does not contain a usable count. Used to report truncated
#'   files, never to limit reading.
#'
#' @param skip_tracks integer, the number of tracks to skip before selecting
#'   any, see \code{\link{read.dti.trk}}.
#'
#' @param bbox numeric vector of length 6 or NULL, a bounding box, see
#'   \code{\link{read.dti.trk}}.
#'
#' @param transform 4x4 numeric matrix or NULL. If given, it is applied to the
#'   coordinates of every track as it is read, before the bounding box filter is
#'   evaluated and before they are stored. This is used to filter in the target
#'   coordinate system without transforming the whole result twice.
#'
#' @return named list with entries \code{coords} (matrix with 3 columns and one
#'   row per point), \code{lengths} (integer vector, points per track),
#'   \code{scalars} (matrix or NULL) and \code{properties} (matrix or NULL).
#'
#' @keywords internal
read.trk.records <- function(fh, endian, n_scalars, n_properties, max_tracks = Inf,
                             expected_tracks = NA_real_, skip_tracks = 0L, bbox = NULL,
                             transform = NULL) {
  values_per_point <- 3L + n_scalars;

  # Coordinates and scalars are stored interleaved in the file, so they are kept
  # in a single buffer and split up at the end.
  data <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  data_capacity <- 0L;
  data_used <- 0L;

  properties <- if (n_properties > 0L) matrix(numeric(0L), nrow = 0L, ncol = n_properties) else NULL;
  properties_capacity <- 0L;

  lengths <- integer(0L);
  num_tracks <- 0L;

  if (!is.finite(max_tracks)) {
    max_tracks <- .Machine$integer.max;
  }

  skip <- as.integer(skip_tracks);
  filtered <- !is.null(bbox) || skip > 0L;
  eof_reached <- FALSE;

  repeat {
    if (num_tracks >= max_tracks) {
      break;
    }

    num_points <- readBin(fh, integer(), n = 1L, size = 4L, endian = endian);
    if (length(num_points) == 0L) {
      eof_reached <- TRUE;
      break;
    }
    if (is.na(num_points) || num_points < 0L) {
      stop(sprintf("Invalid point count %s for track %d, the TRK file is corrupt.\n", num_points, num_tracks + 1L));
    }

    # A track with zero points is legal and 'nibabel' returns it as well, so it
    # is handled like any other track. In particular its properties still have
    # to be consumed, otherwise the stream would get out of sync with the file.
    if (skip > 0L && is.null(bbox)) {
      # Track records are self-delimiting, so a skipped track does not have to
      # be read at all.
      seek(fh, where = 4 * num_points * values_per_point, origin = "current");
      if (n_properties > 0L) {
        seek(fh, where = 4 * n_properties, origin = "current");
      }
      skip <- skip - 1L;
      next;
    }

    if (num_points > 1000000L) {
      # A single streamline with more than a million points is not plausible for
      # real data, so treat this as a corrupt file instead of attempting a huge
      # allocation.
      validate_allocation_size(c(num_points, values_per_point), 8L, label = "a single TRK track record");
    }

    record <- readBin(fh, numeric(), n = num_points * values_per_point, size = 4L, endian = endian);
    if (length(record) != num_points * values_per_point) {
      stop(sprintf("Truncated TRK file: track %d should have %d values, but only %d could be read.\n",
                   num_tracks + 1L, num_points * values_per_point, length(record)));
    }
    record <- matrix(record, ncol = values_per_point, byrow = TRUE);

    track_properties <- NULL;
    if (n_properties > 0L) {
      track_properties <- readBin(fh, numeric(), n = n_properties, size = 4L, endian = endian);
      if (length(track_properties) != n_properties) {
        stop(sprintf("Truncated TRK file: track %d is missing its %d propert%s.\n",
                     num_tracks + 1L, n_properties, if (n_properties == 1L) "y" else "ies"));
      }
    }

    if (skip > 0L) {
      skip <- skip - 1L;
      next;
    }

    if (!is.null(transform) && num_points > 0L) {
      # Transform the coordinates here, so that the bounding box below is
      # evaluated in the same coordinate system as the returned coordinates. The
      # transformed values are the ones that get stored, so the transformation is
      # not applied a second time later on.
      record[, seq_len(3L)] <- apply.affine.to.coords(record[, seq_len(3L), drop = FALSE], transform);
    }

    if (!is.null(bbox)) {
      test_coords <- record[, seq_len(3L), drop = FALSE];
      inside <- test_coords[, 1L] >= bbox[1L] & test_coords[, 1L] <= bbox[2L] &
        test_coords[, 2L] >= bbox[3L] & test_coords[, 2L] <= bbox[4L] &
        test_coords[, 3L] >= bbox[5L] & test_coords[, 3L] <= bbox[6L];
      if (!any(inside)) {
        next;
      }
    }

    # Append the record. Inline on purpose, see the note at the top of this file.
    needed <- data_used + num_points;
    if (num_points > 0L) {
      if (needed > data_capacity) {
        new_capacity <- next.buffer.capacity(needed, data_capacity,
                                             bytes_per_elem = 8 * values_per_point,
                                             label = "the tract coordinates");
        grown <- matrix(0, nrow = new_capacity, ncol = values_per_point);
        if (data_used > 0L) {
          grown[seq_len(data_used), ] <- data[seq_len(data_used), , drop = FALSE];
        }
        data <- grown;
        data_capacity <- new_capacity;
      }
      data[(data_used + 1L):needed, ] <- record;
      data_used <- needed;
    }

    if (n_properties > 0L) {
      needed_properties <- num_tracks + 1L;
      if (needed_properties > properties_capacity) {
        new_capacity <- next.buffer.capacity(needed_properties, properties_capacity,
                                             bytes_per_elem = 8 * n_properties,
                                             label = "the track properties");
        grown <- matrix(0, nrow = new_capacity, ncol = n_properties);
        if (num_tracks > 0L) {
          grown[seq_len(num_tracks), ] <- properties[seq_len(num_tracks), , drop = FALSE];
        }
        properties <- grown;
        properties_capacity <- new_capacity;
      }
      properties[needed_properties, ] <- track_properties;
    }

    needed_lengths <- num_tracks + 1L;
    if (needed_lengths > length(lengths)) {
      new_capacity <- next.buffer.capacity(needed_lengths, length(lengths), bytes_per_elem = 4,
                                           label = "the per-track point counts");
      lengths <- c(lengths, integer(new_capacity - length(lengths)));
    }
    lengths[needed_lengths] <- num_points;
    num_tracks <- needed_lengths;
  }

  if (eof_reached && !filtered && !is.na(expected_tracks) && expected_tracks > 0 && num_tracks < expected_tracks) {
    stop(sprintf("Truncated TRK file: the header states %d track%s, but the file ends after %d.\n",
                 expected_tracks, if (expected_tracks == 1) "" else "s", num_tracks));
  }

  data <- data[seq_len(data_used), , drop = FALSE];
  coords <- data[, seq_len(3L), drop = FALSE];
  scalars <- if (n_scalars > 0L) data[, (3L + 1L):values_per_point, drop = FALSE] else NULL;
  if (n_properties > 0L) {
    properties <- properties[seq_len(num_tracks), , drop = FALSE];
  }
  lengths <- lengths[seq_len(num_tracks)];

  return(list(coords = coords, lengths = lengths, scalars = scalars, properties = properties));
}


#' @title Read fiber tracks from Diffusion Toolkit in trk format.
#'
#' @param filepath character string, path to file in trk format.
#'
#' @param shift_origin logical, whether to apply the half-voxel origin shift when computing the corrected vox2ras matrix. The TRK format stores a matrix that maps to the voxel corner, not the voxel center (as is the NIfTI convention). Set to `TRUE` (the default) to compute the corrected `vox2ras` that maps to voxel centers, as used by TrackVis. Set to `FALSE` if the file was written by DSI Studio, which does not apply this shift. See the notes for details.
#'
#' @param max_tracks numeric, the maximum number of tracks to read. Use `Inf`
#'   (the default) to read all tracks. This allows reading a subset of a very
#'   large tractogram without holding all of it in memory.
#'
#' @param skip_tracks integer, the number of tracks to skip before reading any.
#'   Skipped tracks are never held in memory. Note that the count refers to the
#'   tracks that a reader returns, and that empty tracks (tracks without any
#'   point, which the TRK format allows) are returned as well, e.g., a file with
#'   one regular track, then an empty one, yields the empty track for
#'   \code{skip_tracks = 1}. The only exception is a \code{bbox} filter, which
#'   drops empty tracks, since no point of them can be inside the box.
#'
#' @param bbox numeric vector of length 6 or NULL. If given, only tracks that
#'   have at least one point inside the box are read, the box is given as
#'   `c(xmin, xmax, ymin, ymax, zmin, zmax)`.
#'
#' @param coords character string or NULL, the coordinate system of the returned
#'   track coordinates. One of 'native' (the coordinates exactly as stored in the
#'   file, which for TRK files is usually 'voxelmm' space, in which the
#'   coordinates refer to voxel corners) or 'ras' (RAS+ mm space in which a
#'   coordinate of (0,0,0) is the center of the first voxel, which is what
#'   'nibabel' and DIPY return by default). If `NULL` (the default), the
#'   coordinates are returned as stored, but a warning is raised when the file
#'   stores a transformation that is not the identity, since the coordinates are
#'   then not in RAS space and plotting them directly would produce e.g., a
#'   mirrored brain. Pass `'native'` to silence that warning.
#'
#'   The `bbox` filter is always interpreted in the same coordinate system as the
#'   returned coordinates.
#'
#' @return named list, the parsed file data. The naming of the variables follows the spec at \code{http://trackvis.org/docs/?subsect=fileformat}. The returned header will contain the field `vox2ras` (the raw matrix stored in the TRK file, mapping from mm space to RAS) and, if `shift_origin` is `TRUE`, the additional field `vox2ras_corrected` (the computed matrix mapping from voxel indices to voxel center RAS coordinates). It also contains the entry `coords_space` ('native' or 'ras'), which records the coordinate system the returned tracks are in, so that the result can be passed to `write.dti.trk()` without losing that information. The tracks are stored in an \code{\link{is.fs.tracts}} instance: `tracks[[i]]` returns a list with the entries `coords` (n x 3 matrix), `num_points` (integer), `scalars` (matrix or NULL) and `properties` (numeric vector or NULL) for the i-th track. Use `fs.tracts.lengths(trk$tracks)` to get the number of points of each track, and `as.list(trk$tracks)` to convert to a plain list.
#'
#' @note The 4x4 matrix stored in TRK files (labeled `vox_to_ras` in the spec) is actually a transformation from **mm** space to RAS, not from voxel space to RAS. The TRK format was designed by TrackVis with the assumption that voxels are 1 mm\eqn{^3}, and that coordinates refer to voxel corners rather than centers. To obtain the actual vox2ras matrix (voxel center in RAS), the raw matrix must be combined with a voxel-size scaling and a half-voxel offset correction: `vox2ras_corrected = mm2ras %*% mm_correction %*% vox2mm`, where `vox2mm` scales by the inverse voxel size and `mm_correction` shifts by -0.5 mm. Note that DSI Studio does **not** apply this half-voxel shift, so you may need to set `shift_origin=FALSE` for DSI Studio files.
#'
#' @note Use `coords = 'ras'` to have the transformation applied to the returned coordinates. That transformation additionally accounts for the `voxel_order` stored in the header, like the reference implementation in 'nibabel' does, see \code{\link{trackvis.affine.to.rasmm}}. Note that the matrix stored in the header alone is **not** sufficient to transform the coordinates: it has to be combined with the voxel sizes, the half-voxel offset and the orientation.
#'
#' @examples
#' \dontrun{
#' trk <- read.dti.trk("~/simple.trk")
#' trk2 <- read.dti.trk("~/standard.trk")
#' trk3 <- read.dti.trk("~/complex_big_endian.trk")
#'
#' # Coordinates in RAS+ mm, ready for plotting against an MNI template:
#' trk_ras <- read.dti.trk("~/simple.trk", coords = "ras")
#' }
#'
#' @export
read.dti.trk <- function(filepath, shift_origin = TRUE, max_tracks = Inf, skip_tracks = 0L,
                         bbox = NULL, coords = NULL) {
  trk_header <- read.dti.trk.header(filepath, shift_origin = shift_origin);
  validate.bbox(bbox);

  if (!is.null(coords)) {
    coords <- match.arg(coords, c("native", "ras"));
  }
  apply_affine <- identical(coords, "ras");

  # The affine that maps the stored coordinates to RAS+ mm. Note that this is not
  # simply the matrix stored in the header, see trackvis.affine.to.rasmm().
  affine_ras <- NULL;
  if (apply_affine) {
    affine_ras <- if (shift_origin) trackvis.affine.to.rasmm(trk_header) else trk_header$vox2ras;
  }

  if (is.null(coords) && !is.identity.matrix(trk_header$vox2ras)) {
    warning(sprintf(paste0("The track coordinates in '%s' are not in RAS space: the file stores a ",
                           "transformation to RAS (voxel_order '%s'). The coordinates are returned as ",
                           "stored, pass coords = 'ras' to have the transformation applied, or ",
                           "coords = 'native' to silence this warning.\n"),
                    filepath, trk_header$voxel_order));
  }

  # The 'n_count' header field is 0 if the number of tracks was not stored (see
  # the TRK spec), and reading it can also yield NA for values that do not fit
  # into a 32 bit integer. In both cases the number of tracks is unknown, and
  # tracks are read until the end of the file instead of silently returning
  # nothing.
  num_tracks_stored <- trk_header$n_count;
  if (is.na(num_tracks_stored) || num_tracks_stored <= 0L) {
    num_tracks_stored <- NA_real_;
    num_tracks_to_read <- max_tracks;
  } else {
    num_tracks_to_read <- min(num_tracks_stored, max_tracks);
  }

  fh <- file(filepath, "rb");
  on.exit(
    {
      close(fh);
    },
    add = TRUE
  );
  seek(fh, where = trk_header$hdr_size, origin = "start");

  # When a bounding box is combined with coords='ras', the coordinates have to be
  # transformed before the box is tested. Doing that per track keeps the filter
  # exact without transforming the whole result twice.
  record_transform <- if (apply_affine && !is.null(bbox)) affine_ras else NULL;

  track_data <- read.trk.records(fh, endian = get.dti.trk.endianness(filepath),
                                 n_scalars = trk_header$n_scalars,
                                 n_properties = trk_header$n_properties,
                                 max_tracks = num_tracks_to_read,
                                 expected_tracks = num_tracks_stored,
                                 skip_tracks = skip_tracks,
                                 bbox = bbox,
                                 transform = record_transform);

  track_coords <- track_data$coords;
  if (apply_affine && is.null(record_transform)) {
    track_coords <- apply.affine.to.coords(track_coords, affine_ras);
  }

  trk_header$coords_space <- if (apply_affine) "ras" else "native";

  return(list(
    header = trk_header,
    tracks = fs.tracts(track_coords, track_data$lengths,
                       scalars = track_data$scalars,
                       properties = track_data$properties,
                       kind = "trk")
  ));
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
  fh <- file(filepath, "rb");
  on.exit(
    {
      close(fh);
    },
    add = TRUE
  );

  seek(fh, where = 996L, origin = "start");

  endian <- "little";
  sizeof_hdr_little <- readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if (length(sizeof_hdr_little) == 1L && !is.na(sizeof_hdr_little) && sizeof_hdr_little == 1000L) {
    return(endian);
  } else {
    seek(fh, where = 996L, origin = "start");
    endian <- "big";
    sizeof_hdr_big <- readBin(fh, integer(), n = 1, size = 4, endian = endian);
    if (length(sizeof_hdr_big) == 1L && !is.na(sizeof_hdr_big) && sizeof_hdr_big == 1000L) {
      return(endian);
    } else {
      # Files shorter than 1000 bytes cannot contain a TRK header, so the read
      # above returns nothing at all instead of a wrong value.
      stop(sprintf(paste0("File '%s' not in TRK format (header sizes %s/%s in little/big endian mode, ",
                          "expected 1000).\n"), filepath,
                   .format.header.size(sizeof_hdr_little), .format.header.size(sizeof_hdr_big)));
    }
  }
}


#' @title Format a possibly missing TRK header size for an error message.
#'
#' @param value integer vector, the result of a \code{readBin} call, which is
#'   empty when the file ended before the requested position.
#'
#' @return character string.
#'
#' @keywords internal
.format.header.size <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return("<none>");
  }
  return(as.character(value));
}
