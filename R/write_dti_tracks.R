# Functions to write DTI tract files in MRtrix TCK/TSF and TrackVis TRK format ----
#
# The readers in read_dti_tcktsf.R and read_dti_trk.R return tracks as an
# fs.tracts instance, and the writers in this file accept that, so that a
# tractogram can be read, modified and written back out without losing
# information, and so that subsets of huge tractograms can be exported (the
# typical workflow being: read a few thousand tracks of a 10 GB tractogram and
# write them to a small file).
#
# The TCK and the TSF format share the header format and store a payload of
# interleaved float values, they differ in the number of values per point (3
# coordinates versus one scalar) and in the terminator (an Inf triplet versus no
# terminator at all), see write.mrtrix.streamlines().


#' @title Convert a collection of tracts to an fs.tracts instance.
#'
#' @description Accepts an \code{fs.tracts} instance, a list of numeric matrices
#'   with 3 columns, or a single such matrix (which is then treated as one
#'   tract), and returns an \code{fs.tracts} instance. This is what the writers
#'   use to accept several input types.
#'
#' @param tracts the input, see the description.
#'
#' @param kind character string, 'tck' or 'trk', used for the returned instance.
#'
#' @return an \code{fs.tracts} instance.
#'
#' @examples
#' tracts <- as.fs.tracts(list(matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE)));
#' length(tracts);
#'
#' @export
as.fs.tracts <- function(tracts, kind = "tck") {
  if (is.fs.tracts(tracts)) {
    return(tracts);
  }

  if (is.matrix(tracts)) {
    tracts <- list(tracts);
  }
  if (!is.list(tracts) || length(tracts) == 0L) {
    stop("Parameter 'tracts' must be an fs.tracts instance, a list of matrices, or a matrix.\n");
  }

  lengths <- vapply(tracts, function(track) {
    if (!is.matrix(track) || ncol(track) != 3L || !is.numeric(track)) {
      stop("Every tract must be a numeric matrix with 3 columns.\n");
    }
    return(nrow(track));
  }, integer(1L));

  coords <- if (sum(lengths) == 0L) {
    matrix(numeric(0L), nrow = 0L, ncol = 3L);
  } else {
    do.call(rbind, lapply(tracts, function(track) return(unname(track))));
  };

  return(fs.tracts(coords, lengths, kind = kind));
}


#' @title Build the text header of an MRtrix streamlines file.
#'
#' @description MRtrix streamlines files (TCK for the tracks, TSF for per-point
#'   values along the tracks) share one header format, they only differ in the
#'   identifier line and in the meaning of the payload. The header is
#'   ASCII text, and its length is stored within the header itself as the offset
#'   at which the binary payload starts.
#'
#' @param header_id character string, the file type identifier, one of 'mrtrix
#'   tracks' (TCK) or 'mrtrix track scalars' (TSF).
#'
#' @param entries named list of additional header entries.
#'
#' @param datatype character string, the datatype.
#'
#' @param count integer, the number of streamlines.
#'
#' @param offset numeric, the byte offset at which the payload starts.
#'
#' @return character string, the header including the terminating newline.
#'
#' @keywords internal
build.mrtrix.header <- function(header_id, entries, datatype, count, offset) {
  # 'derived' is not a file property but the entry that the header readers add to
  # describe the payload they found, so it is dropped as well: passing a header
  # that was read from a file back to a writer would otherwise store it.
  reserved <- c("id", "datatype", "count", "file", "derived");
  lines <- c(header_id,
             paste0("datatype: ", datatype),
             sprintf("count: %d", count));
  if (length(entries) > 0L) {
    for (entry_name in names(entries)) {
      if (!entry_name %in% reserved) {
        lines <- c(lines, sprintf("%s: %s", entry_name, as.character(entries[[entry_name]])[1L]));
      }
    }
  }
  lines <- c(lines, sprintf("file: . %d", offset), "END");
  return(paste0(paste(lines, collapse = "\n"), "\n"));
}


#' @title Determine the length of an MRtrix streamlines header.
#'
#' @description The length of the header determines the data offset that is
#'   stored *inside* the header, so the two have to be reconciled: the offset is
#'   written with a number of digits that depends on its own value, which can
#'   change the length of the header. Starting from an offset guess and
#'   recomputing until it is stable always terminates, since the length only
#'   ever grows with the number of digits and that number is bounded.
#'
#' @inheritParams build.mrtrix.header
#'
#' @return named list with entries \code{text} (the header) and \code{offset}.
#'
#' @keywords internal
build.mrtrix.header.stable <- function(header_id, entries, datatype, count) {
  offset <- 100L;
  text <- build.mrtrix.header(header_id, entries, datatype, count, offset);
  for (attempt in seq_len(10L)) {
    new_offset <- nchar(text, type = "bytes");
    if (new_offset == offset) {
      return(list(text = text, offset = offset));
    }
    offset <- new_offset;
    text <- build.mrtrix.header(header_id, entries, datatype, count, offset);
  }
  stop("Internal error: could not determine the length of the MRtrix header.\n");
}


#' @title Write the payload of an MRtrix streamlines file.
#'
#' @description Writes the concatenated per-point values of all streamlines,
#'   separated by a NaN value after every streamline, as the TCK and TSF formats
#'   require. The data are written in chunks of streamlines, so that the extra
#'   memory needed does not depend on the size of the tractogram. This is shared
#'   by the TCK and the TSF writer, which differ only in the number of values per
#'   point and in the terminator they append.
#'
#' @param con a connection opened in binary write mode.
#'
#' @param values numeric matrix, the concatenated per-point values of all
#'   streamlines, with one column per value (3 coordinates for the TCK format,
#'   one scalar for the TSF format).
#'
#' @param lengths integer vector, the number of points of each streamline.
#'
#' @param dsize integer, the number of bytes per value (4 or 8).
#'
#' @param endian character string, 'little' or 'big'.
#'
#' @param terminator numeric vector or NULL. If given, it is written after the
#'   last streamline (the TCK format appends a vector of Inf values, the TSF
#'   format has no terminator, so the NaN delimiter of the last streamline
#'   already ends the file).
#'
#' @param chunk_tracks integer, the number of streamlines that are converted and
#'   written at once.
#'
#' @return the number of streamlines written, invisibly.
#'
#' @keywords internal
write.mrtrix.streamlines <- function(con, values, lengths, dsize, endian, terminator = NULL, chunk_tracks = 10000L) {
  values <- as.matrix(values);
  num_values_per_point <- ncol(values);
  num_tracks <- length(lengths);

  written <- 0L;
  while (written < num_tracks) {
    track_indices <- seq.int(written + 1L, min(written + chunk_tracks, num_tracks));
    chunk_lengths <- lengths[track_indices];
    chunk_values <- subset.groups(values, lengths, track_indices)$points;

    # The row of a point in the output: the points of a track are shifted down by
    # one row for the NaN delimiter of each preceding track in this chunk.
    out <- matrix(NaN, nrow = sum(chunk_lengths) + length(chunk_lengths), ncol = num_values_per_point);
    if (sum(chunk_lengths) > 0L) {
      point_rows <- rep.int(group.start.rows(chunk_lengths) + seq_along(chunk_lengths) - 1L,
                            chunk_lengths) + sequence(chunk_lengths) - 1L;
      out[point_rows, ] <- chunk_values;
    }
    writeBin(as.numeric(t(out)), con, size = dsize, endian = endian);
    written <- written + length(track_indices);
  }

  if (!is.null(terminator)) {
    writeBin(as.numeric(terminator), con, size = dsize, endian = endian);
  }

  return(invisible(num_tracks));
}


#' @title Parse and validate the datatype of an MRtrix streamlines file.
#'
#' @description The TCK and TSF formats store float values of 32 or 64 bit, in
#'   either byte order.
#'
#' @param datatype character string, the datatype.
#'
#' @return named list with entries \code{dsize} (bytes per value) and
#'   \code{endian} ('little' or 'big').
#'
#' @keywords internal
parse.mrtrix.write.datatype <- function(datatype) {
  valid_datatypes <- c("Float32BE", "Float32LE", "Float64BE", "Float64LE");
  if (!datatype %in% valid_datatypes) {
    stop(sprintf("Invalid 'datatype' '%s', must be one of %s.\n", datatype, paste(valid_datatypes, collapse = ", ")));
  }
  return(list(dsize = if (startsWith(datatype, "Float64")) 8L else 4L,
              endian = if (endsWith(datatype, "BE")) "big" else "little"));
}


#' @title Convert scalar values for streamlines to a list of vectors.
#'
#' @description Normalizes the several input forms accepted by
#'   \code{\link{write.dti.tsf}} to a list of numeric vectors, one per
#'   streamline, and checks the lengths for consistency.
#'
#' @param tracts the input, see \code{\link{write.dti.tsf}}.
#'
#' @param lengths integer vector or NULL, the number of values per streamline.
#'   Only used when \code{tracts} is a plain vector of values.
#'
#' @return named list with entries \code{values} (list of numeric vectors) and
#'   \code{lengths} (integer vector).
#'
#' @keywords internal
as.tsf.scalars <- function(tracts, lengths = NULL) {
  values <- tracts;

  if (is.fs.tracts(tracts)) {
    scalars <- tracts$scalars;
    if (is.null(scalars)) {
      stop(paste0("The 'tracts' instance contains no per-point values. Use fs.tracts(coords, lengths, scalars = values) ",
                  "to attach them, or pass the values themselves (with 'lengths').\n"));
    }
    if (is.null(dim(scalars))) {
      scalars <- matrix(scalars, ncol = 1L);
    }
    if (ncol(scalars) != 1L) {
      stop(sprintf(paste0("A TSF file stores exactly one value per point, but the 'tracts' instance has %d columns of scalars.",
                          " Select one of them first, e.g. write.dti.tsf(tracts$scalars[, 1L, drop = FALSE], filepath, lengths = fs.tracts.lengths(tracts)).\n"),
                   ncol(scalars)));
    }
    return(list(values = as.numeric(scalars),
                lengths = fs.tracts.lengths(tracts)));
  }

  if (is.list(tracts) && !is.null(tracts$merged) && !is.null(tracts$lengths)) {
    # The 'scalars' entry of the result of read.dti.tsf(), so that a scalar file
    # can be read and written back without any conversion in between.
    values <- tracts$merged;
    lengths <- tracts$lengths;
  } else if (is.list(tracts)) {
    if (!all(vapply(tracts, is.numeric, logical(1L))) || any(vapply(tracts, function(x) !is.null(dim(x)), logical(1L)))) {
      stop("Parameter 'tracts' must be an fs.tracts instance, a list of numeric vectors, or a numeric vector (with 'lengths').\n");
    }
    lengths <- vapply(tracts, length, integer(1L));
    values <- unlist(tracts, use.names = FALSE);
  } else if (!is.numeric(tracts) || !is.null(dim(tracts))) {
    stop("Parameter 'tracts' must be an fs.tracts instance, a list of numeric vectors, or a numeric vector (with 'lengths').\n");
  }

  if (is.null(lengths)) {
    stop(paste0("Parameter 'lengths' is required when the values are given as a plain vector, since a TSF file stores ",
                "no track boundaries: the number of values per track has to be stated separately.\n"));
  }
  lengths <- as.integer(lengths);
  if (any(is.na(lengths)) || any(lengths < 0L)) {
    stop("Parameter 'lengths' must be a non-negative integer vector.\n");
  }
  values <- as.numeric(values);
  if (sum(lengths) != length(values)) {
    stop(sprintf("Inconsistent 'lengths': the lengths sum up to %d but there are %d values.\n", sum(lengths), length(values)));
  }

  return(list(values = values, lengths = lengths));
}


#' @title Write tracks to a file in MRtrix TCK format.
#'
#' @description Writes streamlines in the TCK format described at
#'   \code{https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html}.
#'   The format stores a triplet of NaN values between streamlines and a triplet
#'   of Inf values at the end. The output is read by MRtrix and by
#'   \code{\link{read.dti.tck}}.
#'
#' @param tracts the tracks to write, either an \code{fs.tracts} instance or a
#'   list of numeric matrices with 3 columns, see \code{\link{write.dti.trk}}.
#'
#' @param filepath character string, the path of the file to write.
#'
#' @param datatype character string, one of 'Float32LE' (the default, and what
#'   MRtrix writes), 'Float32BE', 'Float64LE' or 'Float64BE'.
#'
#' @param gzip logical or NULL, whether to gzip-compress the output. If
#'   \code{NULL} (the default), the file is compressed when the file name ends in
#'   '.gz'.
#'
#' @param header named list of additional header entries to store in the file,
#'   e.g., the header of the file the tracks were read from. The entries 'id',
#'   'datatype', 'count', 'file' and 'derived' are always computed by the readers
#'   and cannot be set.
#'
#' @return the file path, invisibly.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck", max_tracks = 1000);
#' write.dti.tck(tck$tracks, "first_1000.tck", header = tck$header);
#'
#' # Round trip through a compressed file:
#' write.dti.tck(tck$tracks, "copy.tck.gz");
#' }
#'
#' @note Tracts without any point cannot be represented in the TCK format: they
#'   are written as a bare delimiter, which every reader (including this
#'   package and 'nibabel') drops again, so the file reads back with fewer
#'   tracts than it was written from. Writing such a file raises a warning.
#'   \code{\link{write.dti.trk}} preserves empty tracts.
#'
#' @export
write.dti.tck <- function(tracts, filepath, datatype = "Float32LE", gzip = NULL, header = list()) {
  dtype_info <- parse.mrtrix.write.datatype(datatype);
  dsize <- dtype_info$dsize;
  endian <- dtype_info$endian;

  if (is.null(gzip)) {
    gzip <- endsWith(tolower(filepath), ".gz");
  }

  tract_data <- as.fs.tracts(tracts, kind = "tck");
  coords <- fs.tracts.coords(tract_data);
  lengths <- fs.tracts.lengths(tract_data);
  num_tracks <- length(lengths);

  if (any(lengths == 0L)) {
    # An empty tract is written as a single delimiter, which is exactly what a
    # reader drops again (an empty tract and a tract of length zero cannot be
    # told apart in the format, and 'nibabel' drops them as well). Warn instead
    # of silently writing a file that reads back with fewer tracts.
    warning(sprintf(paste0("%d of the %d tracts to write are empty (they have no points).",
                           " The TCK format cannot store them, they will be missing when the file is read",
                           " back. Use write.dti.trk() if the empty tracts have to be preserved.\n"),
                    sum(lengths == 0L), num_tracks), call. = FALSE);
  }

  header_text <- build.mrtrix.header.stable("mrtrix tracks", header, datatype, num_tracks);

  con <- if (gzip) gzfile(filepath, open = "wb") else file(filepath, open = "wb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  writeBin(charToRaw(header_text$text), con);

  # The payload consists of the coordinates, with a NaN triplet between the
  # tracks and an Inf triplet at the end.
  write.mrtrix.streamlines(con, coords, lengths, dsize, endian, terminator = c(Inf, Inf, Inf));

  return(invisible(filepath));
}


#' @title Write per-point track values to a file in MRtrix TSF format.
#'
#' @description The TSF format stores one scalar value per point of a
#'   streamline, e.g., the fractional anisotropy, the distance along the track
#'   or a value sampled from an image at the point coordinates. It is the
#'   companion format of the TCK format: a TSF file contains no coordinates and
#'   no track boundaries, it is just a stream of values that has to be read
#'   together with the tractogram it describes. The number of values per track
#'   is therefore required to write the file, and a TSF file without the
#'   matching TCK file is meaningless to every reader (MRtrix checks this, see
#'   the note below).
#'
#' @param tracts the values to write. This can be an \code{fs.tracts} instance
#'   whose \code{scalars} entry holds a single column of values (as returned by
#'   \code{\link{read.dti.trk}} for a file with one scalar, see
#'   \code{\link{fs.tracts}} to construct one), the \code{scalars} entry of the
#'   result of \code{\link{read.dti.tsf}} (so that a scalar file can be read and
#'   written back), a list of numeric vectors (one per track), or a single
#'   numeric vector of all values concatenated (in which case \code{lengths} is
#'   required).
#'
#' @param filepath character string, the path of the file to write.
#'
#' @param lengths integer vector or NULL, the number of values per track. This
#'   is ignored unless \code{tracts} is a plain vector, and has to be given in
#'   that case.
#'
#' @param datatype character string, one of 'Float32LE' (the default, and what
#'   MRtrix writes), 'Float32BE', 'Float64LE' or 'Float64BE'.
#'
#' @param gzip logical or NULL, whether to gzip-compress the output. If
#'   \code{NULL} (the default), the file is compressed when the file name ends in
#'   '.gz'.
#'
#' @param header named list of additional header entries to store in the file,
#'   e.g., the header of the file the tracks were read from. The entries 'id',
#'   'datatype', 'count', 'file' and 'derived' are always computed by the readers
#'   and cannot be set.
#'
#' @return the file path, invisibly.
#'
#' @examples
#' # A TSF file stores one value per point. Since the format contains no track
#' # boundaries, the track lengths have to be provided:
#' tsff <- tempfile(fileext = ".tsf");
#' values_by_track <- list(c(0.1, 0.2, 0.3), c(0.4, 0.5));
#' write.dti.tsf(values_by_track, tsff);
#' read.dti.tsf(tsff)$scalars$scalar_list;
#'
#' # The same file can be written from one vector of values and the lengths:
#' write.dti.tsf(c(0.1, 0.2, 0.3, 0.4, 0.5), tsff, lengths = c(3L, 2L));
#'
#' \dontrun{
#' # Read the values of a track scalar file, modify them and write them back:
#' tsf <- read.dti.tsf("brain.tsf");
#' tsf$scalars$merged <- tsf$scalars$merged * 2;
#' write.dti.tsf(tsf$scalars, "brain_doubled.tsf");
#'
#' # Sample an image along the tracks of a tractogram and store the result. The
#' # values of a TRK file that has one scalar are accepted as they are:
#' trk <- read.dti.trk("brain.trk");
#' write.dti.tsf(trk$tracks, "brain.trk.tsf");
#' }
#'
#' @seealso \code{\link{read.dti.tsf}}, \code{\link{write.dti.tck}}
#'
#' @note The TSF format stores a NaN value after every track, and unlike the TCK
#'   format it has no Inf terminator: the reader relies on the delimiters to
#'   split the value stream into tracks. A file whose values are not delimited
#'   exactly like the tracks of the tractogram can therefore not be detected as
#'   broken by this package, but MRtrix reports the mismatch of the track counts
#'   when the file is used (e.g., in \code{tcksample} or \code{tsfvalidate}).
#'
#' @export
write.dti.tsf <- function(tracts, filepath, lengths = NULL, datatype = "Float32LE", gzip = NULL, header = list()) {
  dtype_info <- parse.mrtrix.write.datatype(datatype);
  dsize <- dtype_info$dsize;
  endian <- dtype_info$endian;

  if (is.null(gzip)) {
    gzip <- endsWith(tolower(filepath), ".gz");
  }

  scalar_data <- as.tsf.scalars(tracts, lengths = lengths);
  values <- scalar_data$values;
  lengths <- scalar_data$lengths;
  num_tracks <- length(lengths);

  if (any(lengths == 0L)) {
    warning(sprintf(paste0("%d of the %d tracks to write are empty (they have no points).",
                           " The TSF format cannot store them, they will be missing when the file is read",
                           " back, and the remaining values cannot be matched to the tracks of the tractogram",
                           " file anymore. Remove the empty tracks from both files instead of writing this file.\n"),
                    sum(lengths == 0L), num_tracks), call. = FALSE);
  }

  header_text <- build.mrtrix.header.stable("mrtrix track scalars", header, datatype, num_tracks);

  con <- if (gzip) gzfile(filepath, open = "wb") else file(filepath, open = "wb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  writeBin(charToRaw(header_text$text), con);

  # One value per point, with a NaN after *every* track, including the last one:
  # unlike the TCK format, the TSF format has no Inf terminator, the final NaN
  # of the last track is what ends the file. MRtrix rejects a file that ends
  # with an Inf value (or that has no delimiter after the last track) with a
  # track count mismatch.
  write.mrtrix.streamlines(con, values, lengths, dsize, endian, terminator = NULL);

  return(invisible(filepath));
}


#' @title Write the 1000 byte header of a TRK file.
#'
#' @param con a connection opened in binary write mode.
#'
#' @param header named list, the header fields to write. Missing fields get
#'   defaults.
#'
#' @param num_tracks integer, the value for the 'n_count' field.
#'
#' @param endian character string, 'little' or 'big'.
#'
#' @return \code{NULL}, invisibly. The header is written to \code{con}.
#'
#' @keywords internal
write.trk.header <- function(con, header, num_tracks, endian) {
  get_or_default <- function(name, default) {
    value <- header[[name]];
    if (is.null(value) || (length(value) == 1L && is.na(value))) {
      return(default);
    }
    return(value);
  }

  write_fixed_chars <- function(value, n) {
    if (is.null(value) || length(value) == 0L || is.na(value[1L])) {
      value <- "";
    }
    bytes <- charToRaw(substr(as.character(value[1L]), 1L, n));
    writeBin(c(bytes, as.raw(rep(0, n - length(bytes))))[seq_len(n)], con);
  }

  write_fixed_chars("TRACK", 6L);
  writeBin(as.integer(get_or_default("dim", c(1L, 1L, 1L))), con, size = 2, endian = endian);
  writeBin(as.numeric(get_or_default("voxel_size", c(1, 1, 1))), con, size = 4, endian = endian);
  writeBin(as.numeric(get_or_default("origin", c(0, 0, 0))), con, size = 4, endian = endian);
  writeBin(as.integer(get_or_default("n_scalars", 0L)), con, size = 2, endian = endian);
  write_fixed_chars(get_or_default("scalar_names", ""), 200L);
  writeBin(as.integer(get_or_default("n_properties", 0L)), con, size = 2, endian = endian);
  write_fixed_chars(get_or_default("property_names", ""), 200L);
  writeBin(as.numeric(t(get_or_default("vox2ras", diag(4L)))), con, size = 4, endian = endian);
  write_fixed_chars(get_or_default("reserved", ""), 444L);
  write_fixed_chars(get_or_default("voxel_order", "RAS"), 4L);
  write_fixed_chars(get_or_default("pad2", ""), 4L);
  writeBin(as.numeric(get_or_default("image_orientation_patient", rep(0, 6L))), con, size = 4, endian = endian);
  write_fixed_chars(get_or_default("pad1", ""), 2L);
  writeBin(as.raw(as.integer(get_or_default("invert_x", 0L))), con);
  writeBin(as.raw(as.integer(get_or_default("invert_y", 0L))), con);
  writeBin(as.raw(as.integer(get_or_default("invert_z", 0L))), con);
  writeBin(as.raw(as.integer(get_or_default("swap_xy", 0L))), con);
  writeBin(as.raw(as.integer(get_or_default("swap_yz", 0L))), con);
  writeBin(as.raw(as.integer(get_or_default("swap_zx", 0L))), con);
  writeBin(as.integer(num_tracks), con, size = 4, endian = endian);
  writeBin(as.integer(get_or_default("version", 2L)), con, size = 4, endian = endian);
  writeBin(as.integer(get_or_default("hdr_size", 1000L)), con, size = 4, endian = endian);
  return(invisible(NULL));
}


#' @title Write tracks to a file in TrackVis TRK format.
#'
#' @description Writes streamlines in the TRK format described at
#'   \code{http://trackvis.org/docs/?subsect=fileformat}. The output is read by
#'   TrackVis, by DSI Studio and by \code{\link{read.dti.trk}}.
#'
#' @param tracts the tracks to write, either an \code{fs.tracts} instance as
#'   returned in the \code{tracks} entry of \code{\link{read.dti.trk}}, or a list
#'   of numeric matrices with 3 columns, or a single such matrix. Per-point
#'   scalars and per-track properties are written if the input is an
#'   \code{fs.tracts} instance that has them.
#'
#' @param filepath character string, the path of the file to write.
#'
#' @param header named list or NULL, the header of the file the tracks were read
#'   from, whose metadata (voxel sizes, dimensions, the voxel-to-RAS matrix, the
#'   voxel order, and the scalars and properties of the file) is reused. If
#'   \code{NULL}, a minimal header is written that describes coordinates in 1 mm
#'   isotropic voxels with an identity voxel-to-RAS matrix, which is the case
#'   that needs no coordinate correction at all.
#'
#' @param coords_space character string or NULL, the coordinate system the
#'   tracks are in, either 'native' (the coordinates are written as they are) or
#'   'ras' (the coordinates are RAS+ mm and are transformed back to the space
#'   used in the file, which is what 'nibabel' does when saving). If \code{NULL}
#'   (the default), the value stored in \code{header$coords_space} is used when
#'   present, and 'native' otherwise. This makes a read-write round trip with
#'   \code{coords = 'ras'} work without any further arguments.
#'
#' @param endian character string, 'little' (the default) or 'big'. TrackVis
#'   writes little endian files, big endian support is for reading files written
#'   on old big endian systems.
#'
#' @param gzip logical or NULL, whether to gzip-compress the output. If
#'   \code{NULL} (the default), the file is compressed when the file name ends in
#'   '.gz'. Note that the TrackVis tools and MRtrix do not read compressed track
#'   files, so this is useful for archiving and for passing the file back to
#'   \code{\link{read.dti.trk}}, but not for exchanging it with other software.
#'
#' @return the file path, invisibly.
#'
#' @examples
#' \dontrun{
#' trk <- read.dti.trk("brain.trk");
#' write.dti.trk(trk$tracks, "copy.trk", header = trk$header);
#'
#' # Write RAS coordinates back to a file that uses voxelmm space:
#' trk_ras <- read.dti.trk("brain.trk", coords = "ras");
#' write.dti.trk(trk_ras$tracks, "copy.trk", header = trk_ras$header, coords_space = "ras");
#' }
#'
#' @export
write.dti.trk <- function(tracts, filepath, header = NULL, coords_space = NULL, endian = "little", gzip = NULL) {
  if (!endian %in% c("little", "big")) {
    stop("Parameter 'endian' must be one of 'little' or 'big'.\n");
  }
  if (is.null(header)) {
    header <- list();
  }

  if (is.null(gzip)) {
    gzip <- endsWith(tolower(filepath), ".gz");
  }

  if (is.null(coords_space)) {
    coords_space <- if (!is.null(header$coords_space)) header$coords_space else "native";
  }
  coords_space <- match.arg(coords_space, c("native", "ras"));

  tract_data <- as.fs.tracts(tracts, kind = "trk");
  coords <- fs.tracts.coords(tract_data);
  lengths <- fs.tracts.lengths(tract_data);
  scalars <- tract_data$scalars;
  properties <- tract_data$properties;

  num_scalars <- if (is.null(scalars)) 0L else ncol(scalars);
  num_properties <- if (is.null(properties)) 0L else ncol(properties);
  values_per_point <- 3L + num_scalars;

  if (coords_space == "ras") {
    if (is.null(header$vox2ras)) {
      stop("Parameter 'coords_space' is 'ras' but the header contains no 'vox2ras' matrix to transform the coordinates back with.\n");
    }
    affine_ras <- trackvis.affine.to.rasmm(header);
    coords <- apply.affine.to.coords(coords, solve(affine_ras));
    header$coords_space <- "native";
  }

  write_header <- header;
  write_header$n_scalars <- num_scalars;
  write_header$n_properties <- num_properties;
  if (is.null(write_header$vox2ras)) {
    write_header$vox2ras <- diag(4L);
  }
  if (is.null(write_header$voxel_order)) {
    write_header$voxel_order <- "RAS";
  }

  con <- if (gzip) gzfile(filepath, "wb") else file(filepath, "wb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  write.trk.header(con, write_header, num_tracks = length(lengths), endian = endian);

  # Write the records in chunks of tracks to keep the extra memory bounded.
  chunk_tracks <- 10000L;
  written <- 0L;
  while (written < length(lengths)) {
    track_indices <- seq.int(written + 1L, min(written + chunk_tracks, length(lengths)));
    selected <- subset.groups(coords, lengths, track_indices);
    chunk_lengths <- selected$lengths;
    record_values <- selected$points;

    if (num_scalars > 0L) {
      # Scalars are interleaved with the coordinates in the file.
      record_values <- cbind(record_values, scalars[rep.int(group.start.rows(lengths)[track_indices], chunk_lengths) +
                                                      sequence(chunk_lengths) - 1L, , drop = FALSE]);
    }

    # Write track by track, since every record is prefixed with its point count.
    row_idx <- 1L;
    for (local_idx in seq_along(track_indices)) {
      num_points <- chunk_lengths[local_idx];
      writeBin(as.integer(num_points), con, size = 4L, endian = endian);
      if (num_points > 0L) {
        writeBin(as.numeric(t(record_values[row_idx:(row_idx + num_points - 1L), , drop = FALSE])),
                 con, size = 4L, endian = endian);
        row_idx <- row_idx + num_points;
      }
      if (num_properties > 0L) {
        writeBin(as.numeric(properties[track_indices[local_idx], ]), con, size = 4L, endian = endian);
      }
    }
    written <- written + length(track_indices);
  }

  return(invisible(filepath));
}
