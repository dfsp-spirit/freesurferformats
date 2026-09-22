# Functions for reading DTI tracking data from files in MRtrix TCK/TSF --------
#
# Format documentation:
# https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html
#
# Both formats are an ASCII header in 'key: value' form, terminated by a line
# reading 'END', followed by a binary payload of float32 or float64 values. The
# payload is a flat stream of numbers in which non-finite values act as
# separators: a triplet of NaN values separates two streamlines and a triplet
# of Inf values terminates the data (for TSF files it is a single NaN and a
# single Inf). Three properties of this layout drive the implementation below:
#
# 1) The payload is only readable as a strictly sequential stream of values, it
#    cannot be sliced at arbitrary byte offsets the way a fixed-record file can.
#    We therefore never derive the payload length from the file size: the file
#    may be gzip-compressed (QSIRecon and other pipelines write ".tck.gz"), in
#    which case the size on disk has no relation to the number of values.
# 2) The end of the data may be marked by the Inf terminator. That marker is not
#    always present in files found in the wild (see the note in
#    read.mrtrix.stream), so its absence is not treated as an error.
# 3) Whole-brain tractograms are huge (10 million streamlines are ~10 GB of
#    float32 data), so the reader works in chunks, never materializes the
#    payload twice, and can skip and filter streamlines without keeping the
#    skipped ones in memory.
#
# Note on the buffers: they are grown and filled with inline
# `data[from:to, ] <- rows` assignments deliberately. Handing a buffer to a
# helper function and assigning the result back would give the object a second
# reference, and R would then copy the entire buffer on every single append,
# which turns reading into an O(n^2) operation for large files.


#' @title Check whether a file is gzip-compressed, based on its magic bytes.
#'
#' @description Uses the gzip magic number (0x1f 0x8b) rather than the file
#'   extension, since files are regularly renamed or stripped of their
#'   extension. Reading a gzip-compressed payload from a plain \code{file()}
#'   connection silently produces garbage.
#'
#' @param filepath character string, path to the file to check.
#'
#' @return logical, TRUE if the file starts with the gzip magic number.
#'
#' @keywords internal
is.gzip.file <- function(filepath) {
  fh <- file(filepath, "rb");
  on.exit(
    {
      close(fh);
    },
    add = TRUE
  );
  magic <- readBin(fh, what = "raw", n = 2L);
  return(identical(magic, as.raw(c(0x1f, 0x8b))));
}


#' @title Open a connection that transparently handles gzip compression.
#'
#' @param filepath character string, path to the file.
#'
#' @param gzipped logical, whether the file is gzip-compressed (see
#'   \code{is.gzip.file}).
#'
#' @param mode character string, 'r' for text or 'rb' for binary.
#'
#' @return a connection, call \code{close()} on it when done.
#'
#' @keywords internal
#'
#' @exportS3Method NULL
open.maybe.gzip <- function(filepath, gzipped, mode = "rb") {
  if (identical(mode, "rb")) {
    if (gzipped) {
      return(gzfile(filepath, open = "rb"));
    }
    return(file(filepath, open = "rb"));
  }
  if (gzipped) {
    return(gzfile(filepath, open = "r"));
  }
  return(file(filepath, open = "r"));
}


#' @title Check whether a header line terminates the MRtrix header.
#'
#' @param line character string, a single header line.
#'
#' @return logical, TRUE if the line reads 'END' (ignoring surrounding
#'   whitespace and a possible DOS line ending).
#'
#' @keywords internal
is.mrtrix.end.line <- function(line) {
  return(grepl("^[[:space:]]*END[[:space:]]*$", line, useBytes = TRUE));
}


#' @title Read the ASCII header of an MRtrix TCK/TSF file.
#'
#' @description Reads lines up to and including the terminating 'END' line. The
#'   number of header lines is not limited, headers of real files contain a
#'   variable number of entries (command history, comments, ROI specifications).
#'
#' @param filepath character string, path to the file.
#'
#' @return named list with entries \code{lines} (the raw header lines),
#'   \code{gzipped} (logical) and \code{header} (the parsed key-value pairs).
#'
#' @keywords internal
read.mrtrix.header <- function(filepath) {
  gzipped <- is.gzip.file(filepath);

  con <- open.maybe.gzip(filepath, gzipped, mode = "r");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  # Read single lines until 'END' so that the binary payload is never read as
  # text (which would emit warnings about embedded nuls and could fail on
  # invalid multibyte sequences).
  max_header_lines <- 100000L;
  lines <- character(0L);
  found_end <- FALSE;
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE);
    if (length(line) == 0L) {
      break;
    }
    lines <- c(lines, line);
    if (is.mrtrix.end.line(line)) {
      found_end <- TRUE;
      break;
    }
    if (length(lines) >= max_header_lines) {
      break;
    }
  }

  if (length(lines) == 0L) {
    stop(sprintf("File '%s' is not in TCK/TSF format: file is empty.\n", filepath));
  }
  if (!found_end) {
    stop(sprintf("File '%s' is not in TCK/TSF format: header has no terminating 'END' line.\n", filepath));
  }

  return(list(lines = lines, gzipped = gzipped, header = parse.mrtrix.header(lines, filepath)));
}


#' @title Parse the key-value pairs of an MRtrix TCK/TSF header.
#'
#' @description Splits each header line at its first colon. Values may contain
#'   colons (e.g., in \code{command_history} entries), they are preserved. Lines
#'   without a colon are ignored, as the MRtrix reference implementation does.
#'   Keys that occur more than once are collected into a vector.
#'
#' @param lines character vector, the header lines, including the first line and
#'   the terminating 'END' line.
#'
#' @param filepath character string, the path of the file, used in error
#'   messages only.
#'
#' @return named list, the parsed header.
#'
#' @keywords internal
parse.mrtrix.header <- function(lines, filepath = "") {
  id <- trimws(lines[1L]);
  if (!id %in% c("mrtrix tracks", "mrtrix track scalars")) {
    stop(sprintf("File '%s' is not in TCK/TSF format: invalid first line.\n", filepath));
  }

  end_index <- which(is.mrtrix.end.line(lines))[1L];
  body <- if (end_index > 2L) lines[2L:(end_index - 1L)] else character(0L);

  key_values <- list();
  for (line in body) {
    line <- trimws(line);
    if (!nzchar(line)) {
      next;
    }
    colon_pos <- regexpr(":", line, fixed = TRUE)[1L];
    if (colon_pos < 1L) {
      next;
    }
    key <- trimws(substr(line, 1L, colon_pos - 1L));
    if (!nzchar(key)) {
      next;
    }
    value <- trimws(substr(line, colon_pos + 1L, nchar(line)));
    key_values[[key]] <- c(key_values[[key]], value);
  }

  parsed <- lapply(key_values, function(values) utils::type.convert(values, as.is = TRUE));

  return(c(list(id = id), parsed));
}


#' @title Parse the 'file' entry of an MRtrix TCK/TSF header.
#'
#' @description The entry has the form 'file: . OFFSET', where the file name part
#'   must be a single dot for single-file TCK/TSF files and the offset gives the
#'   byte position at which the binary payload starts.
#'
#' @param header named list, the parsed header.
#'
#' @param filepath character string, the path of the file, used in messages only.
#'
#' @return named list with entries \code{filename_part} (character) and
#'   \code{offset} (numeric).
#'
#' @keywords internal
parse.mrtrix.file.entry <- function(header, filepath = "") {
  file_entry <- header$file;
  if (is.null(file_entry) || !nzchar(trimws(file_entry[1L]))) {
    stop(sprintf("File '%s' is not in TCK/TSF format: header has no 'file' entry.\n", filepath));
  }

  parts <- strsplit(trimws(file_entry[1L]), "[[:space:]]+")[[1L]];
  if (length(parts) < 2L) {
    stop(sprintf("File '%s' is not in TCK/TSF format: invalid 'file' entry '%s'.\n", filepath, file_entry[1L]));
  }
  if (!identical(parts[1L], ".")) {
    stop("Multi-file TCK/TSF files are not supported, the file name part of the 'file' entry must be '.'.\n");
  }

  offset <- suppressWarnings(as.numeric(parts[2L]));
  if (is.na(offset) || offset < 0) {
    stop(sprintf("File '%s' is not in TCK/TSF format: invalid data offset '%s'.\n", filepath, parts[2L]));
  }

  return(list(filename_part = parts[1L], offset = offset));
}


#' @title Validate and describe the datatype entry of an MRtrix TCK/TSF header.
#'
#' @param header named list, the parsed header.
#'
#' @param filepath character string, the path of the file, used in messages only.
#'
#' @return named list with entries \code{dsize} (bytes per value) and
#'   \code{endian} ('little' or 'big').
#'
#' @keywords internal
parse.mrtrix.datatype <- function(header, filepath = "") {
  datatype <- header$datatype;
  if (is.null(datatype) || !nzchar(trimws(datatype[1L]))) {
    stop(sprintf("File '%s' is not in TCK/TSF format: header has no 'datatype' entry.\n", filepath));
  }
  datatype <- trimws(datatype[1L]);
  valid_datatypes <- c("Float32BE", "Float32LE", "Float64BE", "Float64LE");
  if (!datatype %in% valid_datatypes) {
    stop(sprintf("File '%s' is not in TCK/TSF format: invalid datatype '%s', must be one of %s.\n",
                 filepath, datatype, paste(valid_datatypes, collapse = ", ")));
  }
  return(list(
    dsize = if (startsWith(datatype, "Float64")) 8L else 4L,
    endian = if (endsWith(datatype, "BE")) "big" else "little"
  ));
}


#' @title Parse the 'count' entry of an MRtrix TCK/TSF header.
#'
#' @description The entry is optional: the MRtrix documentation lists only
#'   'file' and 'datatype' as required header keys, and explicitly notes that
#'   the value may not reflect the number of streamlines actually stored, e.g.,
#'   when a command was terminated prematurely. The value is therefore used as a
#'   hint only, never to decide how much data to read.
#'
#' @param header named list, the parsed header.
#'
#' @return numeric, the stored count, or NA if it is absent or not parseable.
#'
#' @keywords internal
parse.mrtrix.count <- function(header) {
  count_entry <- header$count;
  if (is.null(count_entry) || !nzchar(trimws(count_entry[1L]))) {
    return(NA_real_);
  }
  count <- suppressWarnings(as.numeric(trimws(count_entry[1L])));
  if (is.na(count) || count < 0) {
    return(NA_real_);
  }
  return(count);
}


#' @title Compute the next capacity for a growing result buffer.
#'
#' @description Validates that the data actually needed fits into the configured
#'   allocation limit, then returns the new buffer capacity to allocate. The
#'   capacity grows geometrically to keep the number of reallocations
#'   logarithmic, but it is capped at the limit: an overshoot of the growth must
#'   not make a read fail that would have fit into memory, and it must not
#'   exceed the limit either.
#'
#' @param needed numeric, the number of rows that the buffer must hold.
#'
#' @param capacity numeric, the current capacity.
#'
#' @param bytes_per_elem numeric, bytes per row (for integer vectors this is the
#'   size of a single element).
#'
#' @param label character string or NULL, description used in error messages.
#'
#' @return numeric, the new capacity.
#'
#' @keywords internal
next.buffer.capacity <- function(needed, capacity, bytes_per_elem = 8, label = NULL) {
  validate_allocation_size(c(needed), bytes_per_elem, label = label);

  new_capacity <- max(needed, 2 * capacity, 65536);

  max_bytes <- get_max_alloc_bytes();
  if (is.finite(max_bytes)) {
    max_rows <- max_bytes / bytes_per_elem;
    if (max_rows < new_capacity) {
      new_capacity <- max(needed, floor(max_rows));
    }
  }

  return(new_capacity);
}


#' @title Check which rows of a matrix consist of finite values only.
#'
#' @param mat numeric matrix.
#'
#' @return logical vector with one entry per row.
#'
#' @usage finite.rows(mat)
#'
#' @keywords internal
finite.rows <- function(mat) {
  finite <- rep(TRUE, nrow(mat));
  for (col_idx in seq_len(ncol(mat))) {
    finite <- finite & is.finite(mat[, col_idx]);
  }
  return(finite);
}


#' @title Check which rows of a matrix consist of infinite values only.
#'
#' @param mat numeric matrix.
#'
#' @return logical vector with one entry per row.
#'
#' @usage infinite.rows(mat)
#'
#' @keywords internal
infinite.rows <- function(mat) {
  infinite <- rep(TRUE, nrow(mat));
  for (col_idx in seq_len(ncol(mat))) {
    infinite <- infinite & is.infinite(mat[, col_idx]);
  }
  return(infinite);
}


#' @title Split one chunk of MRtrix payload values into streamlines.
#'
#' @description A streamline is a maximal run of finite rows. A run that is not
#'   followed by a separator belongs to the next chunk and is returned as
#'   \code{pending} instead. Runs that contain no points at all (two consecutive
#'   separators, which MRtrix writes for empty streamlines) are dropped, which
#'   is what the 'nibabel' reader does as well.
#'
#' @param mat numeric matrix with one column per point value, the rows of the
#'   chunk, already truncated at the end-of-file marker if the chunk contains
#'   one.
#'
#' @param pending numeric matrix, the points of the streamline that is still
#'   being assembled from the previous chunk.
#'
#' @param finish_pending logical, whether the chunk ended in the end-of-file
#'   marker. In that case the pending streamline is complete and is returned as
#'   a regular streamline, and it must not be dropped.
#'
#' @return named list with entries \code{points} (matrix holding the completed
#'   streamlines, concatenated along the rows), \code{lengths} (integer vector
#'   with the number of points of each completed streamline) and \code{pending}
#'   (matrix, the points of the trailing incomplete streamline).
#'
#' @keywords internal
#'
#' @exportS3Method NULL
split.mrtrix.chunk <- function(mat, pending, finish_pending = FALSE) {
  values_per_point <- ncol(mat);
  empty_mat <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  points <- empty_mat;
  lengths <- integer(0L);

  separator_idx <- which(!finite.rows(mat));

  if (length(separator_idx) == 0L) {
    # No separator at all: the whole chunk belongs to the pending streamline.
    pending <- rbind(pending, mat);
  } else {
    first_sep <- separator_idx[1L];
    last_sep <- separator_idx[length(separator_idx)];

    # Rows before the first separator continue the pending streamline.
    if (first_sep > 1L) {
      pending <- rbind(pending, mat[seq_len(first_sep - 1L), , drop = FALSE]);
    }

    # The pending streamline is complete now.
    if (nrow(pending) > 0L) {
      points <- pending;
      lengths <- nrow(pending);
      pending <- empty_mat;
    }

    # Between the first and the last separator all streamlines are complete and
    # the region starts and ends with a point row, so it contains whole
    # streamlines only. They are collected in one go.
    if (last_sep > first_sep + 1L) {
      region <- (first_sep + 1L):(last_sep - 1L);
      middle_finite <- finite.rows(mat[region, , drop = FALSE]);
      runs <- rle(middle_finite);
      middle_points <- mat[region, , drop = FALSE][rep(runs$values, runs$lengths), , drop = FALSE];
      points <- rbind(points, middle_points);
      lengths <- c(lengths, runs$lengths[runs$values]);
    }

    # Rows after the last separator start a new streamline.
    pending <- if (last_sep < nrow(mat)) {
      mat[(last_sep + 1L):nrow(mat), , drop = FALSE];
    } else {
      empty_mat;
    }
  }

  if (finish_pending && nrow(pending) > 0L) {
    points <- rbind(points, pending);
    lengths <- c(lengths, nrow(pending));
    pending <- empty_mat;
  }

  return(list(points = points, lengths = as.integer(lengths), pending = pending));
}


#' @title Read the data payload of an MRtrix TCK/TSF file.
#'
#' @description Reads the payload sequentially in chunks and splits it into
#'   groups of \code{values_per_point} finite values, which are the points of the
#'   individual streamlines.
#'
#'   The end of the data is normally marked by a group of Inf values, but that
#'   marker is not always present: the TSF test data shipped with this package
#'   has none, and the MRtrix reference reader treats the plain end of the file
#'   as a normal end of the streamline data as well. Its absence is therefore not
#'   an error here, it is only reported as a warning when the header states a
#'   larger number of streamlines than were found, i.e., when data really is
#'   missing.
#'
#' @param con a connection opened in binary read mode.
#'
#' @param offset numeric, byte offset of the payload.
#'
#' @param dsize integer, bytes per value (4 for Float32, 8 for Float64).
#'
#' @param endian character string, 'little' or 'big'.
#'
#' @param values_per_point integer, 3 for TCK (xyz triples) and 1 for TSF.
#'
#' @param max_groups numeric, stop after this many groups have been selected.
#'   Use \code{Inf} to read everything.
#'
#' @param skip_groups integer, the number of streamlines to skip before
#'   selecting any. Skipped streamlines are never held in memory.
#'
#' @param bbox numeric vector of length 6 or NULL. If given, only streamlines
#'   that have at least one point inside the box are selected. The box is given
#'   as \code{c(xmin, xmax, ymin, ymax, zmin, zmax)} in the coordinate system
#'   of the stored data.
#'
#' @param chunk_values integer, number of values to read per chunk.
#'
#' @param filepath character string, the path of the file, used in messages only.
#'
#' @param gzipped logical, whether the connection reads a gzip-compressed file.
#'   R cannot seek on gzip connections, so the header bytes are then skipped by
#'   reading and discarding them instead.
#'
#' @param expected_groups numeric, the number of groups (streamlines) stated in
#'   the header, or NA if the header does not contain a usable count. Only used
#'   to warn about a possibly truncated file, never to limit reading.
#'
#' @return named list with entries \code{data} (matrix with
#'   \code{values_per_point} columns holding the concatenated groups),
#'   \code{lengths} (integer vector, number of points per group) and
#'   \code{terminator_seen} (logical).
#'
#' @keywords internal
read.mrtrix.stream <- function(con, offset, dsize, endian, values_per_point, max_groups = Inf,
                               skip_groups = 0L, bbox = NULL, chunk_values = 4e6,
                               filepath = "", gzipped = FALSE, expected_groups = NA_real_) {
  if (offset > 0) {
    if (gzipped) {
      # R cannot seek on a gzfile connection: the underlying zlib gzseek() fails
      # with 'invalid or incomplete compressed data' when the stream is being
      # decompressed, and it only warns instead of failing loudly, so this would
      # silently read from the wrong position. The header is small compared to
      # the payload, so the offset is reached by reading and discarding the
      # header bytes instead.
      skip <- offset;
      while (skip > 0) {
        skipped <- readBin(con, what = "raw", n = as.integer(min(skip, 1e6)));
        if (length(skipped) == 0L) {
          stop(sprintf("File '%s' is truncated: the %d header bytes could not be skipped.\n",
                       filepath, offset));
        }
        skip <- skip - length(skipped);
      }
    } else {
      seek(con, where = offset, origin = "start");
    }
  }

  if (!is.finite(max_groups)) {
    max_groups <- .Machine$integer.max;
  }
  validate.bbox(bbox);

  data <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  data_capacity <- 0L;
  data_used <- 0L;

  lengths <- integer(0L);
  num_groups <- 0L;

  pending <- matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
  leftover <- numeric(0L);

  terminator_seen <- FALSE;
  stopped_early <- FALSE;
  unfiltered <- (skip_groups == 0L && is.null(bbox));

  repeat {
    chunk <- readBin(con = con, what = numeric(), n = chunk_values, size = dsize, endian = endian);
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

    # A group of infinities terminates the data. Anything after it is ignored.
    first_infinite <- match(TRUE, infinite.rows(mat));
    if (!is.na(first_infinite)) {
      terminator_seen <- TRUE;
      mat <- if (first_infinite > 1L) {
        mat[seq_len(first_infinite - 1L), , drop = FALSE];
      } else {
        matrix(numeric(0L), nrow = 0L, ncol = values_per_point);
      }
    }

    chunk_groups <- split.mrtrix.chunk(mat, pending, finish_pending = terminator_seen);
    pending <- chunk_groups$pending;
    new_points <- chunk_groups$points;
    new_lengths <- chunk_groups$lengths;

    # Apply the subset selection to the streamlines that completed in this chunk.
    if (skip_groups > 0L && length(new_lengths) > 0L) {
      to_drop <- min(skip_groups, length(new_lengths));
      skip_groups <- skip_groups - to_drop;
      keep_groups <- if (to_drop >= length(new_lengths)) integer(0L) else seq.int(to_drop + 1L, length(new_lengths));
      selected <- subset.groups(new_points, new_lengths, keep_groups);
      new_points <- selected$points;
      new_lengths <- selected$lengths;
    }
    if (!is.null(bbox) && length(new_lengths) > 0L) {
      inside <- groups.in.bbox(new_points, new_lengths, bbox);
      if (!all(inside)) {
        selected <- subset.groups(new_points, new_lengths, which(inside));
        new_points <- selected$points;
        new_lengths <- selected$lengths;
      }
    }

    # Append the selected streamlines of this chunk. Inline on purpose, see the
    # note at the top of this file.
    if (nrow(new_points) > 0L) {
      needed <- data_used + nrow(new_points);
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
      data[(data_used + 1L):needed, ] <- new_points;
      data_used <- needed;
    }
    if (length(new_lengths) > 0L) {
      needed <- num_groups + length(new_lengths);
      if (needed > length(lengths)) {
        new_capacity <- next.buffer.capacity(needed, length(lengths), bytes_per_elem = 4,
                                             label = "the per-tract point counts");
        lengths <- c(lengths, integer(new_capacity - length(lengths)));
      }
      lengths[(num_groups + 1L):needed] <- as.integer(new_lengths);
      num_groups <- needed;
    }

    if (terminator_seen) {
      break;
    }
    if (num_groups >= max_groups) {
      stopped_early <- TRUE;
      break;
    }
  }

  if (!terminator_seen && !stopped_early && unfiltered &&
      !is.na(expected_groups) && expected_groups > 0 && num_groups < expected_groups) {
    # Running out of data without hitting the end-of-file marker is not an error
    # by itself, but if the header states a larger number of streamlines than
    # were found, then data really is missing.
    warning(sprintf(paste0("File '%s' has no end-of-file marker and stores %d streamline(s), but the ",
                           "header states %s: the file may be truncated.\n"),
                    filepath, num_groups, format(expected_groups, scientific = FALSE)));
  }

  # Drop everything beyond the requested number of groups.
  if (num_groups > max_groups) {
    num_groups <- as.integer(max_groups);
    lengths <- lengths[seq_len(num_groups)];
    data_used <- sum(lengths);
  }

  data <- data[seq_len(data_used), , drop = FALSE];
  lengths <- lengths[seq_len(num_groups)];

  return(list(data = data, lengths = lengths, terminator_seen = terminator_seen));
}


#' @title Validate a bounding box argument.
#'
#' @param bbox numeric vector of length 6 or NULL.
#'
#' @return TRUE, invisibly. Stops if the box is invalid.
#'
#' @keywords internal
validate.bbox <- function(bbox) {
  if (is.null(bbox)) {
    return(invisible(TRUE));
  }
  if (!is.numeric(bbox) || length(bbox) != 6L || any(is.na(bbox))) {
    stop("Parameter 'bbox' must be a numeric vector of length 6: c(xmin, xmax, ymin, ymax, zmin, zmax).\n");
  }
  if (any(bbox[c(1L, 3L, 5L)] > bbox[c(2L, 4L, 6L)])) {
    stop("Invalid 'bbox': the minimum must not be larger than the maximum for any axis.\n");
  }
  return(invisible(TRUE));
}


#' @title Open the payload of an MRtrix TCK/TSF file for reading.
#'
#' @description Parses and validates the header and returns everything needed to
#'   read the payload. The caller is responsible for closing \code{con}.
#'
#' @param filepath character string, path to the file.
#'
#' @param max_tracks numeric, the maximum number of tracks that will be read.
#'   Used to decide whether an up-front allocation check makes sense.
#'
#' @return named list with entries \code{con}, \code{header}, \code{offset},
#'   \code{dsize}, \code{endian}, \code{values_per_point}, \code{gzipped},
#'   \code{is_tck} and \code{expected_groups}.
#'
#' @keywords internal
#'
#' @exportS3Method NULL
open.mrtrix.payload <- function(filepath, max_tracks = Inf) {
  file_header <- read.mrtrix.header(filepath);
  header <- file_header$header;
  gzipped <- file_header$gzipped;

  file_info <- parse.mrtrix.file.entry(header, filepath);
  dtype_info <- parse.mrtrix.datatype(header, filepath);
  offset <- file_info$offset;
  dsize <- dtype_info$dsize;
  endian <- dtype_info$endian;
  num_tracks_stored <- parse.mrtrix.count(header);

  if (!gzipped) {
    file_size <- file.size(filepath);
    if (offset > file_size) {
      stop(sprintf(paste0("File '%s' is truncated or not in TCK/TSF format: the header states that the ",
                          "data starts at byte %d, but the file is only %d bytes long.\n"),
                   filepath, offset, file_size));
    }
  }

  # A count of zero describes a file that was written without any streamlines,
  # which is a legitimate (if unusual) state, e.g., the result of writing an
  # empty fs.tracts instance. The header is complete, so such a file is read
  # normally and simply yields no streamlines.

  # If the whole payload is requested from an uncompressed file, then its exact
  # size is known and an allocation that cannot possibly succeed is reported
  # before anything is read. This is done before the connection is opened, so
  # that no connection is left behind when it fails. For compressed files (and
  # for subset reads) the limit is enforced while the buffers grow instead.
  # Callers that stream, i.e. the scanner and the iterator, pass a finite
  # max_tracks and are not affected by this up-front check.
  if (!gzipped && !is.finite(max_tracks)) {
    num_values <- (file.size(filepath) - offset) / dsize;
    validate_allocation_size(c(num_values), 8L,
                             label = sprintf("the data payload of '%s'", filepath));
  }

  con <- open.maybe.gzip(filepath, gzipped, mode = "rb");

  is_tck <- identical(header$id, "mrtrix tracks");

  derived <- list(derived = list(
    filename_part = file_info$filename_part,
    data_offset = offset,
    endian = endian,
    dsize = dsize,
    gzipped = gzipped
  ));

  return(list(
    con = con,
    header = c(derived, header),
    offset = offset,
    dsize = dsize,
    endian = endian,
    values_per_point = if (is_tck) 3L else 1L,
    gzipped = gzipped,
    is_tck = is_tck,
    expected_groups = num_tracks_stored
  ));
}


#' @title Read DTI tracking data from MRtrix TCK and TSF files.
#'
#' @param filepath character string, path to the \code{TCK} or \code{TSF} file
#'   to read. Gzip-compressed files (i.e., \code{.tck.gz}) are supported and the
#'   compression is detected from the file content, not the file name.
#'
#' @param max_tracks numeric, the maximum number of tracks to read. Use
#'   \code{Inf} (the default) to read all tracks. This allows reading a subset
#'   of a very large tractogram without holding all of it in memory.
#'
#' @param skip_tracks integer, the number of tracks to skip before reading any.
#'   Skipped tracks are never held in memory.
#'
#' @param bbox numeric vector of length 6 or NULL. If given, only tracks that
#'   have at least one point inside the box are read, the box is given as
#'   \code{c(xmin, xmax, ymin, ymax, zmin, zmax)} in the coordinate system of
#'   the stored data. This has no effect for TSF files, which store no
#'   coordinates. The number of matching tracks cannot be known in advance, so
#'   for very large files this should be combined with a \code{max_tracks}
#'   value that is known to be safe.
#'
#' @param chunk_values integer, the number of payload values that are read per
#'   chunk. This is an advanced tuning parameter that does not change the result,
#'   only the peak memory usage (at most one chunk is buffered at a time) and the
#'   granularity of the file reads. The default of 4e6 values corresponds to
#'   about 32 MB of doubles. Lower it on a machine with very little free memory.
#'
#' @return named list with entries \code{header} and either \code{tracks} (for
#'   TCK files) or \code{scalars} (for TSF files).
#'
#' @keywords internal
.read.dti.tcktsf <- function(filepath, max_tracks = Inf, skip_tracks = 0L, bbox = NULL, chunk_values = 4e6) {
  payload <- open.mrtrix.payload(filepath, max_tracks = max_tracks);
  con <- payload$con;
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  validate.bbox(bbox);

  stream <- read.mrtrix.stream(con, payload$offset, payload$dsize, payload$endian,
                               payload$values_per_point, max_groups = max_tracks,
                               skip_groups = as.integer(skip_tracks), bbox = bbox,
                               chunk_values = chunk_values,
                               filepath = filepath, gzipped = payload$gzipped,
                               expected_groups = payload$expected_groups);

  if (payload$is_tck) {
    tracks <- fs.tracts(stream$data, stream$lengths, kind = "tck");
    return(list(header = payload$header, tracks = tracks));
  }

  merged <- as.vector(stream$data);
  lengths <- stream$lengths;
  scalar_list <- unname(split(merged, rep.int(seq_along(lengths), lengths)));
  return(list(header = payload$header,
              scalars = list(merged = merged, scalar_list = scalar_list, lengths = lengths)));
}


# Exported functions ------------------------------------------------------

#' @title Read the header of a file in MRtrix 'TCK' or 'TSF' format.
#'
#' @description Reads only the ASCII header of the file, without touching the
#'   binary payload. This is cheap even for huge tractograms and can be used to
#'   inspect a file before deciding whether to read its tracks.
#'
#' @inheritParams .read.dti.tcktsf
#'
#' @return named list, the parsed header. The entries of the 'derived' entry
#'   describe the payload (data offset, datatype, endianness, and whether the
#'   file is gzip-compressed).
#'
#' @examples
#' \dontrun{
#' hdr <- read.dti.tck.header("brain.tck");
#' hdr$count;              # number of streamlines stated in the header
#' hdr$derived$gzipped;    # TRUE for a .tck.gz file
#' }
#'
#' @export
read.dti.tck.header <- function(filepath) {
  file_header <- read.mrtrix.header(filepath);
  header <- file_header$header;
  file_info <- parse.mrtrix.file.entry(header, filepath);
  dtype_info <- parse.mrtrix.datatype(header, filepath);
  return(c(list(derived = list(
    filename_part = file_info$filename_part,
    data_offset = file_info$offset,
    endian = dtype_info$endian,
    dsize = dtype_info$dsize,
    gzipped = file_header$gzipped
  )), header));
}


#' @title Read the header of a file in MRtrix 'TSF' format.
#'
#' @description Reads only the ASCII header of the file, see
#'   \code{\link{read.dti.tck.header}}.
#'
#' @inheritParams read.dti.tck.header
#'
#' @return named list, the parsed header.
#'
#' @examples
#' \dontrun{
#' hdr <- read.dti.tsf.header("brain.tsf");
#' }
#'
#' @export
read.dti.tsf.header <- function(filepath) {
  return(read.dti.tck.header(filepath));
}


#' @title Read DTI tracking data from file in MRtrix 'TCK' format.
#'
#' @description Reads streamlines from a TCK file, optionally gzip-compressed.
#' @inheritParams .read.dti.tcktsf
#'
#' @examples
#' \dontrun{
#' tckf <- "~/simple.tck"
#' tck <- read.dti.tck(tckf)
#'
#' # Read only the first 1000 streamlines of a huge tractogram:
#' first_tracks <- read.dti.tck(tckf, max_tracks = 1000)
#'
#' # Read the streamlines passing through a region:
#' region <- read.dti.tck(tckf, bbox = c(-10, 10, -20, 20, 0, 30))
#' }
#'
#' @return named list with entries 'header' and 'tracks'. The tracks are stored
#'   in an \code{\link{is.fs.tracts}} instance: all coordinates are kept in a
#'   single matrix and \code{tracks[[i]]} returns the n x 3 coordinate matrix of
#'   the i-th track. Use \code{fs.tracts.lengths(tck$tracks)} to get the number
#'   of points of each track, and \code{as.list(tck$tracks)} to convert to a
#'   plain list of matrices.
#'
#' @export
read.dti.tck <- function(filepath, max_tracks = Inf, skip_tracks = 0L, bbox = NULL, chunk_values = 4e6) {
  return(.read.dti.tcktsf(filepath, max_tracks = max_tracks, skip_tracks = skip_tracks, bbox = bbox,
                          chunk_values = chunk_values));
}


#' @title Read DTI tracking per-coord data from file in MRtrix 'TSF' format.
#'
#' @description Reads per-vertex track scalars from a TSF file, optionally
#'   gzip-compressed.
#' @inheritParams .read.dti.tcktsf
#'
#' @examples
#' \dontrun{
#' tsff <- "~/simple.tsf"
#' tsf <- read.dti.tsf(tsff)
#' }
#'
#' @return named list with entries 'header' and 'scalars'. The scalar data are
#'   available in 3 representations: 'merged' (a vector of all values), 'lengths'
#'   (the number of values per track) and 'scalar_list' (a list of vectors, one
#'   per track, which is expensive for large files and should be avoided for
#'   whole-brain data in favour of 'merged' and 'lengths').
#'
#' @export
read.dti.tsf <- function(filepath, max_tracks = Inf, skip_tracks = 0L, chunk_values = 4e6) {
  return(.read.dti.tcktsf(filepath, max_tracks = max_tracks, skip_tracks = skip_tracks, bbox = NULL,
                          chunk_values = chunk_values));
}
