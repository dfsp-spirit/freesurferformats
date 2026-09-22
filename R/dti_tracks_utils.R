# Utilities that operate on collections of tracts (streamlines) --------------
#
# These helpers are shared by the TRK and TCK/TSF readers. They deal with the
# "array sequence" layout used throughout the package: all points of all tracts
# concatenated in one matrix, plus one length per tract.
#
# The functions in this file that start with `dti.` are exported and work on
# files rather than on in-memory data: they stream through a tract file without
# ever holding all of it in memory, which is what makes them usable on the
# multi-gigabyte whole-brain tractograms that tractography pipelines produce.


#' @title Compute the first row of every group in a concatenated matrix.
#'
#' @param lengths integer vector, the number of rows of each group.
#'
#' @return integer vector with one entry per group.
#'
#' @keywords internal
group.start.rows <- function(lengths) {
  if (length(lengths) == 0L) {
    return(integer(0L));
  }
  return(cumsum(c(1L, lengths[-length(lengths)])));
}


#' @title Extract selected groups from a concatenated matrix.
#'
#' @param points numeric matrix holding the concatenated rows of all groups.
#'
#' @param lengths integer vector with the number of rows of each group.
#'
#' @param sel integer vector, the indices of the groups to extract.
#'
#' @return named list with entries \code{points} (matrix with the rows of the
#'   selected groups) and \code{lengths} (their lengths).
#'
#' @keywords internal
#'
#' @exportS3Method NULL
subset.groups <- function(points, lengths, sel) {
  sel <- as.integer(sel);
  if (length(sel) == 0L) {
    return(list(points = matrix(numeric(0L), nrow = 0L, ncol = ncol(points)),
                lengths = integer(0L)));
  }
  sel_lengths <- lengths[sel];
  starts <- group.start.rows(lengths)[sel];
  rows <- rep.int(starts, sel_lengths) + sequence(sel_lengths) - 1L;
  return(list(points = points[rows, , drop = FALSE], lengths = sel_lengths));
}


#' @title Check which groups have at least one point inside a box.
#'
#' @param points numeric matrix with 3 columns, the concatenated points.
#'
#' @param lengths integer vector with the number of points of each group.
#'
#' @param bbox numeric vector of length 6:
#'   \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}.
#'
#' @return logical vector with one entry per group.
#'
#' @keywords internal
groups.in.bbox <- function(points, lengths, bbox) {
  if (length(lengths) == 0L) {
    return(logical(0L));
  }
  # rowsum() counts the points inside the box per streamline; it is vectorized,
  # unlike tapply() on a factor with millions of levels. It omits groups that
  # have no points at all, so the result is expanded back to one entry per
  # streamline (an empty streamline has no point, so it is never inside a box).
  inside_group <- logical(length(lengths));
  with_points <- which(lengths > 0L);
  if (length(with_points) > 0L) {
    inside <- points[, 1L] >= bbox[1L] & points[, 1L] <= bbox[2L] &
      points[, 2L] >= bbox[3L] & points[, 2L] <= bbox[4L] &
      points[, 3L] >= bbox[5L] & points[, 3L] <= bbox[6L];
    counts <- as.vector(rowsum(as.integer(inside), rep.int(with_points, lengths[with_points]),
                               reorder = FALSE));
    inside_group[with_points] <- counts > 0L;
  }
  return(inside_group);
}


#' @title Detect the format of a DTI tract file.
#'
#' @param filepath character string, path to the file.
#'
#' @return character string, one of 'tck', 'tsf' or 'trk'.
#'
#' @keywords internal
detect.dti.tract.format <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("File '%s' does not exist.\n", filepath));
  }

  gzipped <- is.gzip.file(filepath);

  # The TRK magic number is the 5 byte string 'TRACK'; the header field it is
  # stored in is 6 bytes and NUL padded. Compressed TRK files are supported as
  # well, so the magic bytes are read through the gzip layer when needed.
  fh <- open.maybe.gzip(filepath, gzipped, mode = "rb");
  magic <- readBin(fh, what = "raw", n = 5L);
  close(fh);
  if (identical(magic, charToRaw("TRACK"))) {
    return("trk");
  }

  con <- open.maybe.gzip(filepath, gzipped, mode = "r");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );
  first_line <- readLines(con, n = 1L, warn = FALSE);
  if (length(first_line) == 1L) {
    first_line <- trimws(first_line);
    if (identical(first_line, "mrtrix tracks")) {
      return("tck");
    }
    if (identical(first_line, "mrtrix track scalars")) {
      return("tsf");
    }
  }

  stop(sprintf("File '%s' is not in TRK, TCK or TSF format.\n", filepath));
}


#' @title Scan a tract file without holding its data in memory.
#'
#' @description Streams through the tract file and either counts the tracts, or
#'   computes the bounding box of all their points, or both. Nothing but the
#'   current chunk is ever held in memory, so this works on files of any size.
#'
#' @inheritParams read.dti.tck
#'
#' @param want character vector, any combination of 'count' and 'bbox'.
#'
#' @param chunk_values integer, number of payload values to read per chunk.
#'
#' @return named list with the entries that were requested:
#'   \code{count} (integer, the number of tracts) and \code{bbox} (numeric
#'   vector of length 6, \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}), or
#'   \code{NULL} for \code{bbox} if no point was found.
#'
#' @keywords internal
scan.dti.tract.file <- function(filepath, want = c("count", "bbox"), chunk_values = 4e6) {
  want <- match.arg(want, c("count", "bbox"), several.ok = TRUE);
  format <- detect.dti.tract.format(filepath);

  if (identical(format, "trk")) {
    return(scan.trk.file(filepath, want));
  }
  return(scan.mrtrix.file(filepath, want, chunk_values = chunk_values));
}


#' @title Scan an MRtrix TCK or TSF file.
#'
#' @param filepath character string, path to the file.
#'
#' @param want character vector, the values to compute, see
#'   \code{scan.dti.tract.file}.
#'
#' @param chunk_values integer, number of payload values to read per chunk.
#'
#' @return named list with entries \code{count} and \code{bbox}.
#'
#' @keywords internal
scan.mrtrix.file <- function(filepath, want, chunk_values = 4e6) {
  want_bbox <- "bbox" %in% want;

  # max_tracks = 1 tells the payload opener that the data is streamed and never
  # held in memory as a whole, so its up-front allocation check does not apply.
  payload <- open.mrtrix.payload(filepath, max_tracks = 1L);
  con <- payload$con;
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  if (want_bbox && !payload$is_tck) {
    stop("TSF files store per-point scalars, not coordinates, so they have no bounding box.\n");
  }

  if (payload$offset > 0) {
    skip.connection.bytes(con, payload$offset, payload$gzipped, filepath);
  }

  values_per_point <- payload$values_per_point;
  pending <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  leftover <- numeric(0L);
  count <- 0L;
  bbox <- NULL;

  repeat {
    chunk <- readBin(con = con, what = numeric(), n = chunk_values, size = payload$dsize,
                     endian = payload$endian);
    if (length(chunk) == 0L) {
      break;
    }

    values <- if (length(leftover) == 0L) chunk else c(leftover, chunk);
    num_values <- length(values);
    num_rows <- num_values %/% values_per_point;
    if (num_rows == 0L) {
      leftover <- values;
      next;
    }
    mat <- matrix(values[seq_len(num_rows * values_per_point)], ncol = values_per_point, byrow = TRUE);
    leftover <- if (num_values > num_rows * values_per_point) {
      values[(num_rows * values_per_point + 1L):num_values];
    } else {
      numeric(0L);
    }

    termin <- match(TRUE, infinite.rows(mat));
    if (!is.na(termin)) {
      mat <- if (termin > 1L) mat[seq_len(termin - 1L), , drop = FALSE] else {
        matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
      }
    }

    groups <- split.mrtrix.chunk(mat, pending, finish_pending = !is.na(termin));
    pending <- groups$pending;
    count <- count + length(groups$lengths);
    if (want_bbox && nrow(groups$points) > 0L) {
      bbox <- merge.bbox(bbox, coord.bbox(groups$points[, seq_len(3L), drop = FALSE]));
    }

    if (!is.na(termin)) {
      break;
    }
  }

  return(list(count = count, bbox = bbox));
}


#' @title Scan a TRK file.
#'
#' @param filepath character string, path to the file.
#'
#' @param want character vector, the values to compute, see
#'   \code{scan.dti.tract.file}.
#'
#' @return named list with entries \code{count} and \code{bbox}.
#'
#' @keywords internal
scan.trk.file <- function(filepath, want) {
  want_bbox <- "bbox" %in% want;

  endian <- get.dti.trk.endianness(filepath);
  trk_header <- read.dti.trk.header(filepath);
  values_per_point <- 3L + trk_header$n_scalars;
  num_properties <- trk_header$n_properties;

  gzipped <- is.gzip.file(filepath);
  con <- open.maybe.gzip(filepath, gzipped, mode = "rb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );
  skip.connection.bytes(con, trk_header$hdr_size, gzipped, filepath);

  count <- 0L;
  bbox <- NULL;

  repeat {
    num_points <- readBin(con, integer(), n = 1L, size = 4L, endian = endian);
    if (length(num_points) == 0L) {
      break;
    }
    if (is.na(num_points) || num_points < 0L) {
      stop(sprintf("Invalid point count %s in TRK file '%s', the file is corrupt.\n", num_points, filepath));
    }
    # A track with zero points is legal and is counted like any other track, see
    # read.trk.records(). Its properties still have to be consumed to stay in
    # sync with the file.

    if (want_bbox) {
      record <- readBin(con, numeric(), n = num_points * values_per_point, size = 4L, endian = endian);
      if (length(record) != num_points * values_per_point) {
        stop(sprintf("Truncated TRK file: track %d is incomplete.\n", count + 1L));
      }
      record <- matrix(record, ncol = values_per_point, byrow = TRUE);
      bbox <- merge.bbox(bbox, coord.bbox(record[, seq_len(3L), drop = FALSE]));
    } else {
      # The records are self-delimiting, so the payload can be skipped over
      # without reading it at all.
      skip.connection.bytes(con, 4 * num_points * values_per_point, gzipped, filepath);
    }
    if (num_properties > 0L) {
      skip.connection.bytes(con, 4 * num_properties, gzipped, filepath);
    }
    count <- count + 1L;
  }

  return(list(count = count, bbox = bbox));
}


#' @title Skip over bytes of a connection, transparently handling gzip.
#'
#' @description Skips forward from the current position of the connection. R
#'   cannot seek on a gzfile connection (the underlying \code{gzseek()} fails
#'   with 'invalid or incomplete compressed data' and only warns instead of
#'   failing loudly), so compressed connections are skipped by reading and
#'   discarding the bytes instead.
#'
#' @param con a connection opened in binary read mode.
#'
#' @param num_bytes numeric, the number of bytes to skip, relative to the
#'   current position of the connection.
#'
#' @param gzipped logical, whether the connection is a gzfile connection, as
#'   reported by \code{is.gzip.file}.
#'
#' @param filepath character string, used in error messages only.
#'
#' @return \code{TRUE}, invisibly.
#'
#' @keywords internal
skip.connection.bytes <- function(con, num_bytes, gzipped, filepath = "") {
  if (num_bytes <= 0) {
    return(invisible(TRUE));
  }
  if (!gzipped) {
    seek(con, where = num_bytes, origin = "current");
    return(invisible(TRUE));
  }

  skip <- num_bytes;
  while (skip > 0) {
    skipped <- readBin(con, what = "raw", n = as.integer(min(skip, 1e6)));
    if (length(skipped) == 0L) {
      stop(sprintf("File '%s' is truncated: %d byte(s) could not be skipped.\n", filepath, num_bytes));
    }
    skip <- skip - length(skipped);
  }
  return(invisible(TRUE));
}


#' @title Compute the bounding box of a set of coordinates.
#'
#' @param coords numeric matrix with 3 columns.
#'
#' @return numeric vector of length 6,
#'   \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}, or NULL if there are no rows.
#'
#' @keywords internal
coord.bbox <- function(coords) {
  if (nrow(coords) == 0L) {
    return(NULL);
  }
  return(as.vector(rbind(apply(coords, 2L, min), apply(coords, 2L, max))));
}


#' @title Merge two bounding boxes.
#'
#' @param bbox1 numeric vector of length 6 or NULL.
#'
#' @param bbox2 numeric vector of length 6 or NULL.
#'
#' @return numeric vector of length 6, the union of the two boxes.
#'
#' @keywords internal
#'
#' @exportS3Method NULL
merge.bbox <- function(bbox1, bbox2) {
  if (is.null(bbox1)) {
    return(bbox2);
  }
  if (is.null(bbox2)) {
    return(bbox1);
  }
  return(c(min(bbox1[1L], bbox2[1L]), max(bbox1[2L], bbox2[2L]),
           min(bbox1[3L], bbox2[3L]), max(bbox1[4L], bbox2[4L]),
           min(bbox1[5L], bbox2[5L]), max(bbox1[6L], bbox2[6L])));
}


# Exported file-level utilities ------------------------------------------------

#' @title Count the tracts in a DTI tract file.
#'
#' @description Counts the tracts in a TRK, TCK or TSF file without reading
#'   their coordinates, so this works on arbitrarily large tractograms with a
#'   small and constant amount of memory. This is the equivalent of
#'   \code{tckinfo <file> -count} in MRtrix.
#'
#' @inheritParams read.dti.tck
#'
#' @param chunk_values integer, the number of payload values to read per chunk.
#'   Advanced tuning parameter, see \code{\link{read.dti.tck}}.
#'
#' @return integer, the number of tracts in the file. Note that this can differ
#'   from the \code{count} entry in the file header, which the MRtrix
#'   documentation explicitly describes as unreliable, and which does not count
#'   empty tracts.
#'
#' @examples
#' \dontrun{
#' dti.track.count("brain.tck");
#' }
#'
#' @export
dti.track.count <- function(filepath, chunk_values = 4e6) {
  return(scan.dti.tract.file(filepath, want = "count", chunk_values = chunk_values)$count);
}


#' @title Compute the bounding box of all tract coordinates in a file.
#'
#' @description Streams through the tract file and returns the bounding box of
#'   all coordinates, without keeping the coordinates in memory. This is useful
#'   to determine the axis limits for plotting a large tractogram.
#'
#' @inheritParams read.dti.tck
#'
#' @param chunk_values integer, the number of payload values to read per chunk.
#'   Advanced tuning parameter, see \code{\link{read.dti.tck}}.
#'
#' @return numeric vector of length 6,
#'   \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}, or \code{NULL} if the file
#'   contains no coordinates. The coordinates are in the coordinate system used
#'   by the file, see the note in \code{\link{read.dti.trk}}.
#'
#' @examples
#' \dontrun{
#' bbox <- dti.track.bbox("brain.tck");
#' }
#'
#' @export
dti.track.bbox <- function(filepath, chunk_values = 4e6) {
  return(scan.dti.tract.file(filepath, want = "bbox", chunk_values = chunk_values)$bbox);
}


#' @title Create an iterator over the tracts of a DTI tract file.
#'
#' @description Reads a TRK, TCK or TSF file tract by tract, so that a
#'   tractogram of any size can be processed with a constant amount of memory.
#'   This is the low-level interface behind \code{\link{dti.track.count}} and
#'   friends, use it when neither reading a subset
#'   (\code{max_tracks}, \code{skip_tracks}, \code{bbox} in
#'   \code{\link{read.dti.tck}}) nor one of the aggregate functions fits your
#'   use case.
#'
#'   The returned object is an environment with the following entries:
#'   \itemize{
#'     \item \code{next.track()}: returns the next tract (a n x 3 matrix for TCK,
#'       a numeric vector of per-point values for TSF, and a list with the
#'       entries \code{coords}, \code{num_points}, \code{scalars} and
#'       \code{properties} for TRK, just like \code{tracks[[i]]} does for the
#'       readers), or \code{NULL} when there are no more tracts.
#'     \item \code{close()}: closes the underlying file connection. It is safe to
#'       call this more than once, and it is also called automatically when the
#'       iterator is garbage collected.
#'     \item \code{tracks.read}: the number of tracts returned so far.
#'     \item \code{filepath}, \code{format}: the file and its detected format.
#'   }
#'
#' @inheritParams read.dti.tck
#'
#' @return the iterator environment, see the description.
#'
#' @examples
#' \dontrun{
#' itr <- dti.track.iterator("brain.tck");
#' total_points <- 0;
#' while (!is.null(track <- itr$next.track())) {
#'   total_points <- total_points + nrow(track);
#' }
#' itr$close();
#' }
#'
#' @export
dti.track.iterator <- function(filepath, skip_tracks = 0L, bbox = NULL, chunk_values = 4e6) {
  format <- detect.dti.tract.format(filepath);
  validate.bbox(bbox);
  if (!is.null(bbox) && identical(format, "tsf")) {
    stop("TSF files store per-point scalars, not coordinates, so 'bbox' cannot be used with them.\n");
  }

  if (identical(format, "trk")) {
    return(trk.track.iterator(filepath, skip_tracks = skip_tracks, bbox = bbox));
  }
  return(mrtrix.track.iterator(filepath, skip_tracks = skip_tracks, bbox = bbox,
                               chunk_values = chunk_values));
}


#' @title Create an iterator over the tracts of an MRtrix TCK or TSF file.
#'
#' @param filepath character string, path to the file.
#'
#' @param skip_tracks integer, the number of tracts to skip.
#'
#' @param bbox numeric vector of length 6 or NULL, a bounding box, see
#'   \code{read.dti.tck}.
#'
#' @param chunk_values integer, the number of payload values to read per chunk.
#'   This only affects the peak memory usage and the I/O granularity of the
#'   iterator, and is rarely needed. For TRK files it is ignored, since those
#'   records are read one at a time.
#'
#' @return the iterator environment.
#'
#' @keywords internal
mrtrix.track.iterator <- function(filepath, skip_tracks = 0L, bbox = NULL, chunk_values = 4e6) {
  # max_tracks = 1 tells the payload opener that the data is streamed and never
  # held in memory as a whole, so its up-front allocation check does not apply.
  payload <- open.mrtrix.payload(filepath, max_tracks = 1L);
  values_per_point <- payload$values_per_point;

  state <- new.env(parent = emptyenv());
  state$con <- payload$con;
  state$offset <- payload$offset;
  state$dsize <- payload$dsize;
  state$endian <- payload$endian;
  state$gzipped <- payload$gzipped;
  state$skip <- as.integer(skip_tracks);
  state$offset_done <- FALSE;
  state$leftover <- numeric(0L);
  state$pending <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  state$buf.points <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  state$buf.starts <- integer(0L);
  state$buf.lengths <- integer(0L);
  state$buf.index <- 0L;
  state$terminator <- FALSE;
  state$eof <- FALSE;

  itr <- new.env(parent = emptyenv());
  itr$filepath <- filepath;
  itr$format <- if (payload$is_tck) "tck" else "tsf";
  itr$tracks.read <- 0L;
  itr$closed <- FALSE;
  itr$bbox <- bbox;
  itr$values_per_point <- values_per_point;
  itr$state <- state;

  itr$next.track <- function() {
    if (itr$closed) {
      return(NULL);
    }
    if (!state$offset_done) {
      skip.connection.bytes(state$con, state$offset, state$gzipped, filepath);
      state$offset_done <- TRUE;
    }

    repeat {
      # Serve the streamlines that were parsed from the chunk that is buffered.
      if (state$buf.index < length(state$buf.lengths)) {
        state$buf.index <- state$buf.index + 1L;
        start <- state$buf.starts[state$buf.index];
        num_points <- state$buf.lengths[state$buf.index];
        points <- state$buf.points[start:(start + num_points - 1L), , drop = FALSE];
        itr$tracks.read <- itr$tracks.read + 1L;
        if (identical(itr$format, "tsf")) {
          return(as.vector(points));
        }
        return(points);
      }
      if (state$eof) {
        return(NULL);
      }

      chunk <- readBin(con = state$con, what = numeric(), n = chunk_values, size = state$dsize,
                       endian = state$endian);
      if (length(chunk) == 0L) {
        # Running out of data is a normal end of the streamlines, see
        # read.mrtrix.stream(). A trailing incomplete streamline is dropped.
        state$eof <- TRUE;
        return(NULL);
      }

      values <- if (length(state$leftover) == 0L) chunk else c(state$leftover, chunk);
      num_values <- length(values);
      num_rows <- num_values %/% values_per_point;
      if (num_rows == 0L) {
        state$leftover <- values;
        next;
      }
      mat <- matrix(values[seq_len(num_rows * values_per_point)], ncol = values_per_point, byrow = TRUE);
      state$leftover <- if (num_values > num_rows * values_per_point) {
        values[(num_rows * values_per_point + 1L):num_values];
      } else {
        numeric(0L);
      }

      termin <- match(TRUE, infinite.rows(mat));
      if (!is.na(termin)) {
        state$terminator <- TRUE;
        mat <- if (termin > 1L) mat[seq_len(termin - 1L), , drop = FALSE] else {
          matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
        };
      }

      groups <- split.mrtrix.chunk(mat, state$pending, finish_pending = state$terminator);
      state$pending <- groups$pending;
      points <- groups$points;
      lengths <- groups$lengths;

      if (state$skip > 0L && length(lengths) > 0L) {
        to_drop <- min(state$skip, length(lengths));
        state$skip <- state$skip - to_drop;
        keep <- if (to_drop >= length(lengths)) integer(0L) else seq.int(to_drop + 1L, length(lengths));
        selected <- subset.groups(points, lengths, keep);
        points <- selected$points;
        lengths <- selected$lengths;
      }
      if (!is.null(itr$bbox) && length(lengths) > 0L) {
        inside <- groups.in.bbox(points, lengths, itr$bbox);
        if (!all(inside)) {
          selected <- subset.groups(points, lengths, which(inside));
          points <- selected$points;
          lengths <- selected$lengths;
        }
      }

      state$buf.points <- points;
      state$buf.lengths <- as.integer(lengths);
      state$buf.starts <- group.start.rows(state$buf.lengths);
      state$buf.index <- 0L;
      if (state$terminator) {
        state$eof <- TRUE;
      }
    }
  };

  itr$close <- function() {
    if (!itr$closed) {
      try(close(state$con), silent = TRUE);
      itr$closed <- TRUE;
    }
    return(invisible(NULL));
  };

  class(itr) <- "dti.track.iterator";
  reg.finalizer(itr, function(e) {
    if (!isTRUE(e$closed)) {
      try(close(e$state$con), silent = TRUE);
      e$closed <- TRUE;
    }
  }, onexit = TRUE);

  return(itr);
}


#' @title Create an iterator over the tracts of a TRK file.
#'
#' @param filepath character string, path to the file.
#'
#' @param skip_tracks integer, the number of tracts to skip.
#'
#' @param bbox numeric vector of length 6 or NULL, a bounding box, see
#'   \code{read.dti.tck}.
#'
#' @return the iterator environment.
#'
#' @keywords internal
trk.track.iterator <- function(filepath, skip_tracks = 0L, bbox = NULL) {
  endian <- get.dti.trk.endianness(filepath);
  trk_header <- read.dti.trk.header(filepath);
  values_per_point <- 3L + trk_header$n_scalars;
  num_properties <- trk_header$n_properties;

  gzipped <- is.gzip.file(filepath);
  con <- open.maybe.gzip(filepath, gzipped, mode = "rb");
  skip.connection.bytes(con, trk_header$hdr_size, gzipped, filepath);

  state <- new.env(parent = emptyenv());
  state$con <- con;
  state$endian <- endian;
  state$gzipped <- gzipped;
  state$skip <- as.integer(skip_tracks);

  itr <- new.env(parent = emptyenv());
  itr$filepath <- filepath;
  itr$format <- "trk";
  itr$tracks.read <- 0L;
  itr$closed <- FALSE;
  itr$bbox <- bbox;
  itr$values_per_point <- values_per_point;
  itr$n_scalars <- trk_header$n_scalars;
  itr$n_properties <- num_properties;
  itr$state <- state;

  itr$next.track <- function() {
    if (itr$closed) {
      return(NULL);
    }

    repeat {
      num_points <- readBin(state$con, integer(), n = 1L, size = 4L, endian = state$endian);
      if (length(num_points) == 0L) {
        return(NULL);
      }
      if (is.na(num_points) || num_points < 0L) {
        stop(sprintf("Invalid point count %s in TRK file '%s', the file is corrupt.\n", num_points, filepath));
      }
      # A track with zero points is legal and is returned like any other track,
      # see read.trk.records(). Its properties still have to be consumed to stay
      # in sync with the file.

      if (state$skip > 0L && is.null(itr$bbox)) {
        # The records are self-delimiting, so a skipped tract does not have to
        # be read at all.
        skip.connection.bytes(state$con, 4 * num_points * values_per_point, state$gzipped, filepath);
        if (num_properties > 0L) {
          skip.connection.bytes(state$con, 4 * num_properties, state$gzipped, filepath);
        }
        state$skip <- state$skip - 1L;
        next;
      }

      record <- readBin(state$con, numeric(), n = num_points * values_per_point, size = 4L,
                        endian = state$endian);
      if (length(record) != num_points * values_per_point) {
        stop(sprintf("Truncated TRK file: a tract is incomplete.\n"));
      }
      record <- matrix(record, ncol = values_per_point, byrow = TRUE);

      properties <- NULL;
      if (num_properties > 0L) {
        properties <- readBin(state$con, numeric(), n = num_properties, size = 4L, endian = state$endian);
        if (length(properties) != num_properties) {
          stop(sprintf("Truncated TRK file: a tract is missing its properties.\n"));
        }
      }

      if (state$skip > 0L) {
        state$skip <- state$skip - 1L;
        next;
      }

      coords <- record[, seq_len(3L), drop = FALSE];
      if (!is.null(itr$bbox)) {
        inside <- coords[, 1L] >= itr$bbox[1L] & coords[, 1L] <= itr$bbox[2L] &
          coords[, 2L] >= itr$bbox[3L] & coords[, 2L] <= itr$bbox[4L] &
          coords[, 3L] >= itr$bbox[5L] & coords[, 3L] <= itr$bbox[6L];
        if (!any(inside)) {
          next;
        }
      }

      scalars <- NULL;
      if (itr$n_scalars > 0L) {
        scalars <- record[, (3L + 1L):values_per_point, drop = FALSE];
      }
      itr$tracks.read <- itr$tracks.read + 1L;
      return(list("scalars" = scalars, "properties" = properties, "coords" = coords,
                  "num_points" = num_points));
    }
  };

  itr$close <- function() {
    if (!itr$closed) {
      try(close(state$con), silent = TRUE);
      itr$closed <- TRUE;
    }
    return(invisible(NULL));
  };

  class(itr) <- "dti.track.iterator";
  reg.finalizer(itr, function(e) {
    if (!isTRUE(e$closed)) {
      try(close(e$state$con), silent = TRUE);
      e$closed <- TRUE;
    }
  }, onexit = TRUE);

  return(itr);
}


#' @title Print a tract file iterator.
#'
#' @param x a \code{dti.track.iterator} instance.
#'
#' @param ... ignored.
#'
#' @return the iterator, invisibly.
#'
#' @export
print.dti.track.iterator <- function(x, ...) {
  cat(sprintf("dti.track.iterator for %s (%s format), %d tract(s) read so far%s\n",
              x$filepath, x$format, x$tracks.read, if (isTRUE(x$closed)) ", closed." else "."));
  cat("Use $next.track() to read the next tract and $close() when done.\n");
  return(invisible(x));
}
