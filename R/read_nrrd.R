# Read volume data in NRRD format (.nrrd / .nhdr) --------------------------------
#
# NRRD (Nearly Raw Raster Data, the native format of the 'teem' tools and of 3D Slicer) stores an
# N-dimensional array as an ASCII header followed by the data, either in the same file (.nrrd) or in
# a separate file that the header names (.nhdr). It is read by 3D Slicer, ITK/SimpleITK, DTI-TK, DSI
# Studio, Paraview and the microscopy tools of the connectomics field, and it is one of the two
# formats in which the ITK world exchanges volumes.
#
# The format is simple, but it has many variants, and all of them are handled here:
#
#   - The data may be attached (in the same file, starting directly after the blank line that ends
#     the header) or detached (named by the 'data file' field, with the path given relative to the
#     header file). The special value 'LIST' means that the following lines of the header name
#     several data files, whose contents are concatenated in the order in which they are listed.
#   - Four encodings exist: 'raw', 'ascii' (also spelled 'text'), 'gzip' and 'bzip2'. R can
#     decompress the two compressed ones itself (gzcon() and memDecompress()), so no dependency is
#     needed. The compressed stream may start directly after the header (announced by the byte skip
#     value -1, which means 'the data is at the end of the file'), which is how the teem tools write
#     compressed files.
#   - The 'line skip' and 'byte skip' fields announce junk between the header and the data. A byte
#     skip of -1 means that the data is at the very end of the file, so it has to be found by its
#     size rather than by an offset.
#   - The whole file may be gzip-compressed ('.nrrd.gz'). Such a file cannot be seeked in, the
#     header is read line by line and the data is read from the same connection.
#   - Space information is optional. The three space axes are described by 'space directions' (one
#     vector per axis, the step in world space per voxel step), 'space origin' (the world position
#     of the *center* of voxel (0,0,0)) and 'space' (which world space that is: RAS, LPS or
#     scanner-xyz). The older 'spacings' field is only used when there are no space directions. A
#     voxel index is 0-based, and the resulting matrix is returned in RAS+ coordinates, the space
#     that the rest of this package (and FreeSurfer) uses.
#
# Everything that the reader does not interpret is preserved in the 'other_fields' entry of the
# header, so that a future writer can reproduce it. The DWI metadata that the teem/DTI-TK/3D Slicer
# tools store in custom fields ('DWMRI_b-value', 'DWMRI_gradient_NNNN' and 'measurement frame') is
# parsed into the 'dwi' entry, so that it can be handed to the gradient table functions of this
# package, see read.dti.gradients().
#
# The reader is validated against two independent implementations, see
# dev_tools/check_nrrd_conversion.R: pynrrd (which is what Python uses, since nibabel has no NRRD
# support) supplies the array shape, the data type and all values for every test file, and ITK
# (through SimpleITK) supplies the geometry (origin, spacing and direction cosines). ITK normalizes
# sheared space directions, which it cannot represent, so files with a shear are only compared
# against pynrrd.


#' @title Parse a NRRD header field name into a lookup key.
#'
#' @description NRRD field names are matched case-insensitively and some fields are spelled in two
#'   ways ('line skip' and 'lineskip', 'data file' and 'datafile'), so a key is computed by
#'   lowercasing the name and dropping everything that is not a letter or a digit.
#'
#' @param name character string, the field name as it occurs in the file.
#'
#' @return character string, the normalized key.
#'
#' @keywords internal
nrrd.field.key <- function(name) {
  return(gsub("[^a-z0-9]", "", tolower(name)));
}


#' @title Look up a field in a parsed NRRD header.
#'
#' @param fields named list, the fields of the header, as returned by \code{nrrd.parse.header}.
#'
#' @param name character string, the field name to look up, e.g. 'space origin'.
#'
#' @param default the value to return when the field is not present.
#'
#' @return the value of the field, or \code{default} when the field is not present.
#'
#' @keywords internal
nrrd.field <- function(fields, name, default = NULL) {
  key <- nrrd.field.key(name);
  if (key %in% names(fields)) {
    return(fields[[key]]);
  }
  return(default);
}


#' @title Convert the NRRD type name of a volume into R data type information.
#'
#' @description The NRRD spec names the data type of the values with a set of aliases for the
#'   standard integer and floating point types ('uint8', 'unsigned char', 'uchar', ...), see
#'   \code{https://teem.sourceforge.net/nrrd/format.html}. The 64 bit integer types have no R
#'   equivalent and are read as doubles, which is exact up to 2^53.
#'
#' @param type_name character string, the value of the 'type' header field.
#'
#' @param filepath character string, the file the header was read from, used in error messages.
#'
#' @return named list with entries \code{what} (the \code{readBin} type), \code{size} (bytes per
#'   value), \code{signed} (logical, for integer types), \code{itemsize} (bytes per value),
#'   \code{r_class} ('integer' or 'double'), \code{is_int64} (logical) and \code{wide} (logical,
#'   whether the type has to be interpreted from its raw bytes instead of being read by
#'   \code{readBin}).
#'
#' @keywords internal
nrrd.type.info <- function(type_name, filepath = "") {
  if (is.null(type_name) || !is.character(type_name) || length(type_name) != 1L) {
    stop(sprintf("NRRD file '%s' has no or an invalid 'type' header field.\n", filepath));
  }
  key <- trimws(tolower(type_name));

  types <- list(
    "i1" = list(what = "integer", size = 1L, signed = TRUE, itemsize = 1L, r_class = "integer", is_int64 = FALSE, wide = FALSE),
    "u1" = list(what = "integer", size = 1L, signed = FALSE, itemsize = 1L, r_class = "integer", is_int64 = FALSE, wide = FALSE),
    "i2" = list(what = "integer", size = 2L, signed = TRUE, itemsize = 2L, r_class = "integer", is_int64 = FALSE, wide = FALSE),
    "u2" = list(what = "integer", size = 2L, signed = FALSE, itemsize = 2L, r_class = "integer", is_int64 = FALSE, wide = FALSE),
    "i4" = list(what = "integer", size = 4L, signed = TRUE, itemsize = 4L, r_class = "integer", is_int64 = FALSE, wide = FALSE),
    "u4" = list(what = "integer", size = 4L, signed = FALSE, itemsize = 4L, r_class = "double", is_int64 = FALSE, wide = TRUE),
    "i8" = list(what = "double", size = 8L, signed = TRUE, itemsize = 8L, r_class = "double", is_int64 = TRUE, wide = TRUE),
    "u8" = list(what = "double", size = 8L, signed = FALSE, itemsize = 8L, r_class = "double", is_int64 = TRUE, wide = TRUE),
    "float" = list(what = "double", size = 4L, signed = TRUE, itemsize = 4L, r_class = "double", is_int64 = FALSE, wide = FALSE),
    "double" = list(what = "double", size = 8L, signed = TRUE, itemsize = 8L, r_class = "double", is_int64 = FALSE, wide = FALSE)
  );

  aliases <- c(
    "signed char" = "i1", "int8" = "i1", "int8_t" = "i1", "char" = "i1",
    "uchar" = "u1", "unsigned char" = "u1", "uint8" = "u1", "uint8_t" = "u1",
    "short" = "i2", "short int" = "i2", "signed short" = "i2", "signed short int" = "i2",
    "int16" = "i2", "int16_t" = "i2",
    "ushort" = "u2", "unsigned short" = "u2", "unsigned short int" = "u2", "uint16" = "u2", "uint16_t" = "u2",
    "int" = "i4", "signed int" = "i4", "int32" = "i4", "int32_t" = "i4",
    "uint" = "u4", "unsigned int" = "u4", "uint32" = "u4", "uint32_t" = "u4",
    "longlong" = "i8", "long long" = "i8", "long long int" = "i8", "signed long long" = "i8",
    "signed long long int" = "i8", "int64" = "i8", "int64_t" = "i8",
    "ulonglong" = "u8", "unsigned long long" = "u8", "unsigned long long int" = "u8",
    "uint64" = "u8", "uint64_t" = "u8",
    "float" = "float", "double" = "double"
  );

  if (key %in% names(aliases)) {
    key <- aliases[[key]];
  }
  if (!key %in% names(types)) {
    stop(sprintf("Unsupported NRRD data type '%s' in file '%s'. Supported types are the integer types (signed and unsigned, 8 to 64 bit) and the floating point types (32 and 64 bit).\n", type_name, filepath));
  }
  info <- types[[key]];
  info$type <- type_name;
  return(info);
}


#' @title Parse a NRRD header field value.
#'
#' @description Converts the string value of a header field into an R value: a number for the
#'   integer and floating point fields, a vector for the list fields, a vector or a matrix for the
#'   vector and matrix fields (the syntax is \code{(1,2,3)}, several vectors separated by
#'   whitespace, and the value \code{none} for a missing vector), and a character vector for the
#'   string list fields.
#'
#' @param key character string, the normalized field name, see \code{nrrd.field.key}.
#'
#' @param value character string, the raw value from the header.
#'
#' @return the parsed value.
#'
#' @keywords internal
nrrd.parse.field.value <- function(key, value) {
  value <- trimws(value);
  if (key %in% c("dimension", "lineskip", "byteskip", "spacedimension")) {
    return(as.integer(value));
  }
  if (key %in% c("min", "max", "oldmin", "oldmax")) {
    return(as.numeric(value));
  }
  if (key == "sizes") {
    return(as.integer(strsplit(value, "[[:space:]]+")[[1L]]));
  }
  if (key %in% c("spacings", "thicknesses", "axismins", "axismaxs")) {
    return(as.numeric(strsplit(value, "[[:space:]]+")[[1L]]));
  }
  if (key %in% c("kinds", "centerings")) {
    return(strsplit(value, "[[:space:]]+")[[1L]]);
  }
  if (key %in% c("labels", "units", "spaceunits")) {
    return(nrrd.parse.quoted.list(value));
  }
  if (key == "spaceorigin") {
    return(nrrd.parse.vector(value));
  }
  if (key == "measurementframe") {
    return(nrrd.parse.matrix(value));
  }
  if (key == "spacedirections") {
    return(nrrd.parse.vector.list(value));
  }
  return(value);
}


#' @title Parse a NRRD vector value like '(1,2,3)'.
#'
#' @param value character string, the field value.
#'
#' @return numeric vector, or NULL for 'none'.
#'
#' @keywords internal
nrrd.parse.vector <- function(value) {
  value <- trimws(value);
  if (identical(tolower(value), "none")) {
    return(NULL);
  }
  if (!startsWith(value, "(") || !endsWith(value, ")")) {
    stop(sprintf("Invalid NRRD vector value '%s', expected parentheses around the components.\n", value));
  }
  return(as.numeric(strsplit(substr(value, 2L, nchar(value) - 1L), ",", fixed = TRUE)[[1L]]));
}


#' @title Parse a NRRD vector list value like '(1,0,0) (0,1,0) (0,0,1)'.
#'
#' @description Used for the 'space directions' field. Any component may be the string 'none',
#'   which marks a non-space axis (e.g. the time axis of a 4D volume).
#'
#' @param value character string, the field value.
#'
#' @return numeric matrix with one row per vector, or NULL for 'none'. Rows of a non-space axis
#'   contain NA.
#'
#' @keywords internal
nrrd.parse.vector.list <- function(value) {
  value <- trimws(value);
  if (identical(tolower(value), "none")) {
    return(NULL);
  }
  # Every vector is either a parenthesized list or the word 'none', so the tokens are matched
  # instead of being split, which keeps the comma separated components together.
  tokens <- regmatches(value, gregexpr("\\([^)]*\\)|none", value, ignore.case = TRUE))[[1L]];
  if (length(tokens) == 0L) {
    stop(sprintf("Invalid NRRD vector list value '%s'.
", value));
  }
  rows <- lapply(tokens, function(token) {
    if (identical(tolower(token), "none")) {
      return(NA_real_);
    }
    return(as.numeric(strsplit(substr(token, 2L, nchar(token) - 1L), ",", fixed = TRUE)[[1L]]));
  });
  num_columns <- max(vapply(rows, length, integer(1L)));
  mat <- matrix(NA_real_, nrow = length(rows), ncol = num_columns);
  for (idx in seq_along(rows)) {
    if (length(rows[[idx]]) == 1L && is.na(rows[[idx]])) {
      next;
    }
    mat[idx, ] <- rows[[idx]];
  }
  return(mat);
}


#' @title Parse a NRRD matrix value like '(1,0,0) (0,1,0) (0,0,1)'.
#'
#' @inheritParams nrrd.parse.vector.list
#'
#' @return numeric matrix, or NULL for 'none'.
#'
#' @keywords internal
nrrd.parse.matrix <- function(value) {
  return(nrrd.parse.vector.list(value));
}


#' @title Parse a quoted NRRD string list.
#'
#' @param value character string, the field value, e.g. 'left right' or '"some label"'.
#'
#' @return character vector.
#'
#' @keywords internal
nrrd.parse.quoted.list <- function(value) {
  value <- trimws(value);
  if (!nzchar(value)) {
    return(character(0L));
  }
  pattern <- "'([^']*)'|\"([^\"]*)\"|([^[:space:]]+)";
  matches <- gregexpr(pattern, value, perl = TRUE);
  tokens <- regmatches(value, matches)[[1L]];
  return(gsub("^['\"]|['\"]$", "", tokens));
}


#' @title Read the ASCII header of a NRRD file.
#'
#' @description Reads the header bytes of a file in NRRD format, up to and including the blank line
#'   that terminates the header, and returns the header lines together with the byte offset at
#'   which the data starts. The whole header is ASCII text, so it is safe to read it as a string;
#'   the binary data behind it is never touched here.
#'
#' @param filepath character string, path to the file.
#'
#' @param max_header_bytes integer, the maximum number of bytes to read while looking for the blank
#'   line that ends the header. NRRD headers are small (a few KB even for large DWI gradient
#'   tables), this limit only exists to keep a malformed file from producing an endless read.
#'
#' @return named list with entries \code{lines} (character vector, the header lines without the
#'   terminating blank line), \code{data_offset} (numeric, the byte offset at which the data starts,
#'   counted from the beginning of the file, or NA for a gzip-compressed file, in which the data
#'   cannot be seeked to), \code{gzipped_file} (logical, whether the whole file is gzip-compressed)
#'   and \code{header_bytes} (integer, the number of bytes the header occupies).
#'
#' @keywords internal
nrrd.read.header.lines <- function(filepath, max_header_bytes = 10L * 1024L * 1024L) {
  gzipped_file <- is.gzip.file(filepath);

  if (gzipped_file) {
    # A gzip-compressed NRRD file cannot be seeked in, so the header is read line by line through
    # the decompression filter, and the data is read from the same connection afterwards.
    con <- gzfile(filepath, open = "rb");
    on.exit(
      {
        try(close(con), silent = TRUE);
      },
      add = TRUE
    );
    lines <- character(0L);
    repeat {
      line <- readLines(con, n = 1L, warn = FALSE);
      if (length(line) == 0L || !nzchar(trimws(line))) {
        break;
      }
      lines <- c(lines, line);
      if (sum(nchar(lines, type = "bytes")) > max_header_bytes) {
        break;
      }
    }
    return(list(lines = lines, data_offset = NA_real_, gzipped_file = TRUE, header_bytes = NA_integer_));
  }

  con <- file(filepath, open = "rb");
  on.exit(
    {
      close(con);
    },
    add = TRUE
  );

  # Read the file in chunks until the blank line that ends the header is found. The terminator is
  # two consecutive line endings, which may be '\n' or '\r\n' each.
  bytes <- raw(0L);
  terminator_end <- NA_integer_;
  repeat {
    chunk <- readBin(con, what = "raw", n = 65536L);
    if (length(chunk) == 0L) {
      break;
    }
    bytes <- c(bytes, chunk);
    for (pattern in list(charToRaw("\r\n\r\n"), charToRaw("\n\n"), charToRaw("\r\n\n"), charToRaw("\n\r\n"))) {
      hits <- grepRaw(pattern, bytes, fixed = TRUE, all = TRUE);
      if (length(hits) > 0L) {
        candidate <- hits[1L] + length(pattern) - 1L;
        if (is.na(terminator_end) || candidate < terminator_end) {
          terminator_end <- candidate;
        }
      }
    }
    if (!is.na(terminator_end) || length(bytes) >= max_header_bytes) {
      break;
    }
  }

  if (is.na(terminator_end)) {
    stop(sprintf("File '%s' is not in NRRD format: the header is not terminated by a blank line.\n", filepath));
  }

  header_text <- rawToChar(bytes[seq_len(terminator_end - 1L)]);
  lines <- strsplit(header_text, "\r?\n", perl = TRUE)[[1L]];
  lines <- lines[nzchar(lines)];

  return(list(lines = lines, data_offset = as.numeric(terminator_end), gzipped_file = FALSE,
              header_bytes = as.integer(terminator_end)));
}


#' @title Parse the header lines of a NRRD file.
#'
#' @description Splits the header lines into an NRRD magic line and a named list of typed field
#'   values. Field names are normalized for lookup (see \code{nrrd.field.key}), the original
#'   spelling is kept in the result. A field may be written as 'name: value' or as 'name:=value',
#'   the latter meaning that the value is a string, which is how the tools that write custom fields
#'   (e.g. the DWI metadata, or pynrrd) store them. The special 'data file: LIST' mode is handled as
#'   well: the lines that follow it, up to the end of the header, are the names of the data files.
#'
#' @param lines character vector, the header lines, see \code{nrrd.read.header.lines}.
#'
#' @param filepath character string, path to the file, used in error messages.
#'
#' @return named list with entries \code{magic} (character string), \code{fields} (named list,
#'   keyed by the normalized field name), \code{field_names} (named character vector, the original
#'   spelling per key) and \code{data_file_names} (character vector, the file names of the LIST
#'   mode, or NULL).
#'
#' @keywords internal
nrrd.parse.header <- function(lines, filepath = "") {
  if (length(lines) == 0L) {
    stop(sprintf("File '%s' is not in NRRD format: the file is empty.\n", filepath));
  }
  magic <- trimws(lines[1L]);
  if (!startsWith(toupper(magic), "NRRD")) {
    stop(sprintf("File '%s' is not in NRRD format: the first line is '%s', expected a NRRD magic line like 'NRRD0005'.\n",
                 filepath, magic));
  }

  fields <- list();
  field_names <- character(0L);
  data_file_names <- NULL;

  for (line in lines[-1L]) {
    line <- sub("\r$", "", line);
    if (!nzchar(trimws(line)) || startsWith(trimws(line), "#")) {
      next;
    }
    if (!is.null(data_file_names)) {
      # In the LIST mode, every remaining line names one data file.
      data_file_names <- c(data_file_names, trimws(line));
      next;
    }
    colon <- regexpr(":", line, fixed = TRUE)[1L];
    if (colon < 1L) {
      # Lines without a colon are not fields. This happens in files written by hand.
      next;
    }
    name <- trimws(substr(line, 1L, colon - 1L));
    value <- substr(line, colon + 1L, nchar(line));
    if (startsWith(value, "=")) {
      # 'name:=value': the value is a string. The '=' is not part of the value.
      value <- substr(value, 2L, nchar(value));
    }
    value <- trimws(value);
    if (!nzchar(name)) {
      next;
    }
    key <- nrrd.field.key(name);
    fields[[key]] <- nrrd.parse.field.value(key, value);
    field_names[key] <- name;
    if (key == "datafile" && identical(toupper(value), "LIST")) {
      data_file_names <- character(0L);
    }
  }

  return(list(magic = magic, fields = fields, field_names = field_names, data_file_names = data_file_names));
}


#' @title Compute the voxel-to-RAS matrix of a NRRD volume.
#'
#' @description NRRD stores the geometry of a volume as one direction vector per space axis
#'   ('space directions'), the world position of the center of voxel (0,0,0) ('space origin') and
#'   the world space these refer to ('space'). Voxel indices are 0-based. The matrix returned here
#'   always maps to RAS+ coordinates (x = right, y = anterior, z = superior), which is the space
#'   used by FreeSurfer, by the NIfTI standard and by the rest of this package: a file that states
#'   the LPS space therefore has the sign of its first two axes flipped.
#'
#' @param fields named list, the parsed header fields.
#'
#' @param dimension integer, the number of dimensions of the volume.
#'
#' @param filepath character string, path to the file, used in warnings.
#'
#' @return named list with entries \code{matrix} (the 4x4 voxel-to-RAS matrix, or NULL when the
#'   file carries no space information at all) and \code{source} (character string, how the matrix
#'   was derived: 'space directions', 'spacings' or NULL).
#'
#' @keywords internal
nrrd.vox2ras <- function(fields, dimension, filepath = "") {
  directions <- nrrd.field(fields, "space directions");
  origin <- nrrd.field(fields, "space origin");
  spacings <- nrrd.field(fields, "spacings");
  space <- nrrd.field(fields, "space");
  space <- if (is.null(space)) NULL else tolower(trimws(space));

  to_ras_flip <- if (is.null(space) || space %in% c("right-anterior-superior", "ras")) {
    c(1, 1, 1);
  } else if (space %in% c("left-posterior-superior", "lps")) {
    c(-1, -1, 1);
  } else {
    # 'scanner-xyz' and unknown spaces: the axes are used as they are, which is what ITK does for
    # them as well (ITK works in LPS, but for these spaces the file does not claim a convention).
    c(1, 1, 1);
  };

  # The matrix that maps the axes of the file to RAS space.
  basis <- NULL;
  source <- NULL;

  if (!is.null(directions) && is.matrix(directions) && nrow(directions) >= 3L) {
    space_rows <- which(rowSums(is.na(directions)) < ncol(directions));
    if (length(space_rows) != 3L) {
      stop(sprintf("File '%s' states %d space directions for a volume with %d dimensions, but exactly 3 space axes are needed to compute the voxel-to-RAS matrix.\n",
                   filepath, length(space_rows), dimension));
    }
    basis <- t(directions[space_rows, seq_len(min(3L, ncol(directions))), drop = FALSE]);
    source <- "space directions";
    if (length(space_rows) < 3L || any(space_rows > 3L)) {
      # The space axes are not the first three axes: the geometry cannot be expressed as a 4x4
      # matrix over the first three voxel axes, so this is reported instead of returning a matrix
      # that describes the wrong axes.
      stop(sprintf("File '%s' has non-space axes before its space axes, which cannot be expressed as a voxel-to-RAS matrix over the first 3 axes.\n", filepath));
    }
  } else if (!is.null(spacings) && length(spacings) >= 3L) {
    # The legacy field: the axes are aligned with the axes of the stated space.
    basis <- diag(as.numeric(spacings[seq_len(3L)]));
    source <- "spacings";
  }

  if (is.null(basis)) {
    return(list(matrix = NULL, source = NULL));
  }

  if (is.null(origin) || length(origin) < 3L) {
    # The format defines the origin to be all zeros when the field is absent.
    origin <- c(0, 0, 0);
  }
  origin <- as.numeric(origin[seq_len(3L)]);

  vox2ras <- diag(4L);
  vox2ras[seq_len(3L), seq_len(3L)] <- basis %*% diag(to_ras_flip);
  vox2ras[seq_len(3L), 4L] <- origin * to_ras_flip;

  return(list(matrix = vox2ras, source = source));
}


#' @title Parse the diffusion metadata of a NRRD header.
#'
#' @description The teem/DTI-TK/3D Slicer convention for diffusion MRI data in NRRD files stores
#'   the b-value in the field 'DWMRI_b-value' and one gradient vector per volume in the fields
#'   'DWMRI_gradient_0000', 'DWMRI_gradient_0001' and so on. The optional 'measurement frame' field
#'   is the rotation that maps the gradient vectors, which are given in the image (voxel) frame,
#'   into the frame in which the gradients were measured. The values are returned exactly as they
#'   are stored, i.e. in the image frame, together with the measurement frame, so that a caller can
#'   apply it (or hand the values to the gradient table functions of this package, which expect
#'   gradients in image axes as well).
#'
#' @param fields named list, the parsed header fields.
#'
#' @param num_volumes integer, the number of volumes (the size of the last axis), used to check the
#'   number of gradients.
#'
#' @return named list with entries \code{b_value} (numeric or NULL), \code{bvec} (numeric matrix
#'   with one row per gradient, or NULL), \code{num_gradients} (integer, 0 when there are none) and
#'   \code{measurement_frame} (3x3 numeric matrix or NULL). Returns NULL when the header contains
#'   no DWI information at all.
#'
#' @keywords internal
nrrd.dwi.info <- function(fields, num_volumes) {
  keys <- names(fields);
  gradient_keys <- grep("^dwmrigradient[0-9]+$", keys, value = TRUE);
  b_value <- nrrd.field(fields, "DWMRI_b-value");
  measurement_frame <- nrrd.field(fields, "measurement frame");

  if (length(gradient_keys) == 0L && is.null(b_value)) {
    return(NULL);
  }

  bvec <- NULL;
  if (length(gradient_keys) > 0L) {
    # Sort by the numeric suffix, so that the order of the fields in the file does not matter.
    indices <- as.integer(sub("^dwmrigradient", "", gradient_keys));
    gradient_keys <- gradient_keys[order(indices)];
    bvec <- t(vapply(gradient_keys, function(key) {
      components <- as.numeric(strsplit(trimws(as.character(fields[[key]])), "[[:space:]]+")[[1L]]);
      if (length(components) != 3L) {
        stop(sprintf("Invalid DWI gradient '%s' in NRRD header: expected 3 components, found %d.\n",
                     fields[[key]], length(components)));
      }
      return(components);
    }, numeric(3L)));
  }

  if (!is.null(bvec) && !is.na(num_volumes) && num_volumes > 0L && nrow(bvec) != num_volumes) {
    warning(sprintf("The NRRD header contains %d DWI gradients but the 4th axis has %d volumes.\n", nrow(bvec), num_volumes));
  }
  if (!is.null(measurement_frame) && !is.matrix(measurement_frame)) {
    if (length(measurement_frame) == 9L) {
      measurement_frame <- matrix(as.numeric(measurement_frame), ncol = 3L);
    } else {
      measurement_frame <- NULL;
    }
  }

  return(list(
    b_value = if (is.null(b_value)) NULL else as.numeric(b_value),
    bvec = bvec,
    num_gradients = if (is.null(bvec)) 0L else nrow(bvec),
    measurement_frame = measurement_frame
  ));
}


#' @title Resolve the data location of a NRRD file.
#'
#' @description Determines where the data of a NRRD file are stored: in the file itself, in a file
#'   named by the 'data file' header field (a path relative to the directory of the header file),
#'   or in the several files that the 'data file: LIST' mode names.
#'
#' @param fields named list, the parsed header fields.
#'
#' @param data_file_names character vector or NULL, the file names of the LIST mode.
#'
#' @param filepath character string, path to the header file.
#'
#' @return character vector of data file paths, or NULL when the data are attached to the header
#'   file.
#'
#' @keywords internal
nrrd.resolve.data.files <- function(fields, data_file_names, filepath) {
  if (is.null(data_file_names)) {
    data_file <- nrrd.field(fields, "data file");
    if (is.null(data_file)) {
      return(NULL);
    }
    if (identical(toupper(trimws(as.character(data_file))), "LIST")) {
      stop(sprintf("File '%s' uses the NRRD 'data file: LIST' mode but lists no data files.\n", filepath));
    }
    data_file_names <- trimws(as.character(data_file));
  }

  base_dir <- dirname(normalizePath(filepath, mustWork = FALSE));
  files <- vapply(data_file_names, function(name) {
    name <- path.expand(name);
    if (!file.exists(name) && !file.exists(file.path(base_dir, name))) {
      stop(sprintf("The NRRD data file '%s' referenced by '%s' does not exist.\n", name, filepath));
    }
    if (file.exists(name)) {
      return(normalizePath(name, mustWork = TRUE));
    }
    return(normalizePath(file.path(base_dir, name), mustWork = TRUE));
  }, character(1L));
  return(unname(files));
}


#' @title Skip whole lines of a connection.
#'
#' @description Used for the NRRD 'line skip' header field, which announces that the data does not
#'   start directly behind the header but behind a number of newline-terminated lines. Unlike
#'   \code{skip.connection.bytes}, this has to work for connections that cannot be seeked in, so the
#'   bytes are read and discarded.
#'
#' @param con a connection opened in binary read mode.
#'
#' @param num_lines integer, the number of lines to skip.
#'
#' @param filepath character string, used in error messages only.
#'
#' @return \code{TRUE}, invisibly.
#'
#' @keywords internal
nrrd.skip.lines <- function(con, num_lines, filepath = "") {
  if (num_lines <= 0L) {
    return(invisible(TRUE));
  }
  skipped <- 0L;
  repeat {
    byte <- readBin(con, what = "raw", n = 1L);
    if (length(byte) == 0L) {
      stop(sprintf("File '%s' is truncated: %d line(s) could not be skipped before the data.\n", filepath, num_lines));
    }
    if (identical(byte, charToRaw("\n"))) {
      skipped <- skipped + 1L;
      if (skipped >= num_lines) {
        break;
      }
    }
  }
  return(invisible(TRUE));
}


#' @title Read the remaining bytes of a connection.
#'
#' @description Reads everything from the current position to the end of the file, in chunks, to keep
#'   the peak memory of the intermediate buffers bounded. This is used for the bzip2 encoding, which
#'   R cannot decompress as a stream.
#'
#' @param con a connection opened in binary read mode.
#'
#' @return raw vector.
#'
#' @keywords internal
nrrd.read.remainder <- function(con) {
  chunks <- list();
  repeat {
    chunk <- readBin(con, what = "raw", n = 1048576L);
    if (length(chunk) == 0L) {
      break;
    }
    chunks[[length(chunks) + 1L]] <- chunk;
  }
  if (length(chunks) == 0L) {
    return(raw(0L));
  }
  return(do.call(c, chunks));
}


#' @title Convert raw bytes of wide integer types into numbers.
#'
#' @description R's \code{readBin} can read integers of 1, 2 and 4 bytes, but an unsigned 4 byte
#'   integer does not fit into R's signed integer type and there is no integer type of 8 bytes at
#'   all, so these types are interpreted here from their raw bytes. The values are computed in
#'   double precision, which is exact up to 2^53 (for the 64 bit types, larger values lose their low
#'   bits, see the note in \code{read.fs.volume.nrrd}).
#'
#' @param bytes raw vector, the bytes of the values.
#'
#' @param type_info named list, the R data type information, see \code{nrrd.type.info}.
#'
#' @param endian character string, 'little' or 'big'.
#'
#' @return numeric vector, one value per 4 or 8 bytes.
#'
#' @keywords internal
nrrd.raw.to.numeric <- function(bytes, type_info, endian) {
  size <- type_info$itemsize;
  mat <- matrix(as.integer(bytes), nrow = size);
  if (endian == "big") {
    mat <- mat[size:1, , drop = FALSE];
  }
  weights <- 256^(0:3);

  if (size == 4L) {
    # Unsigned 4 byte integers: read as a positive number instead of a signed R integer.
    return(as.vector(weights %*% mat));
  }

  low <- as.vector(weights %*% mat[1:4, , drop = FALSE]);
  high_raw <- as.vector(weights %*% mat[5:8, , drop = FALSE]);
  if (type_info$signed) {
    # Two's complement: a high word at or above 2^31 means that the value is negative.
    high <- ifelse(high_raw >= 2^31, high_raw - 2^32, high_raw);
  } else {
    high <- high_raw;
  }
  return(low + high * 2^32);
}


#' @title Read NRRD values from a connection or a file.
#'
#' @description Reads the scalar values of a NRRD data section, dispatching on the encoding. The
#'   connection has to be positioned at the start of the data. For the compressed encodings the
#'   stream starts at the current position: gzip data are read through a decompression filter
#'   (which also works when the stream is preceded by junk, e.g. an ASCII header), bzip2 data have
#'   to be decompressed in one piece, since R has no streaming bzip2 connection.
#'
#' @param filepath character string, path to the file that holds the data.
#'
#' @param data_offset numeric or NA, the byte offset of the data in the file. NA means that the data
#'   start at the current position of a connection that cannot be seeked in.
#'
#' @param num_values numeric, the number of values to read. For the LIST mode this is computed per
#'   file from the file size, see \code{nrrd.read.data}.
#'
#' @param info named list, the parsed header, see \code{read.nrrd.header}.
#'
#' @param type_info named list, the R data type information, see \code{nrrd.type.info}.
#'
#' @param num_values_exact logical, whether \code{num_values} is exact (then a shorter result is an
#'   error) or an upper bound (then whatever is present is returned). The LIST mode uses it to
#'   accept the values of a single data file.
#'
#' @return numeric or integer vector, the values.
#'
#' @keywords internal
nrrd.read.values <- function(filepath, data_offset, num_values, info, type_info, num_values_exact = TRUE) {
  encoding <- tolower(trimws(info$encoding));
  if (encoding %in% c("gzip", "gz")) {
    encoding <- "gzip";
  } else if (encoding %in% c("bzip2", "bz2")) {
    encoding <- "bzip2";
  } else if (encoding %in% c("ascii", "text", "txt")) {
    encoding <- "ascii";
  }
  if (!encoding %in% c("raw", "ascii", "gzip", "bzip2")) {
    stop(sprintf("Unsupported NRRD encoding '%s' in file '%s'. Supported encodings are 'raw', 'ascii', 'gzip' and 'bzip2'.\n",
                 info$encoding, filepath));
  }

  gzipped_file <- isTRUE(info$gzipped_file);
  con <- if (gzipped_file) gzfile(filepath, open = "rb") else file(filepath, open = "rb");
  on.exit(
    {
      try(close(con), silent = TRUE);
    },
    add = TRUE
  );

  if (gzipped_file) {
    # The header was read from this connection in a previous call, so it has to be skipped again
    # (in bytes), since a gzfile connection cannot be seeked in. The header lines end with a
    # newline each, and the blank line that terminates the header adds one more, so the data start
    # behind one newline more than there are header lines.
    nrrd.skip.lines(con, length(info$header_lines) + 1L, filepath);
    nrrd.skip.lines(con, info$line_skip, filepath);
  } else {
    seek(con, where = data_offset, origin = "start");
    nrrd.skip.lines(con, info$line_skip, filepath);
  }
  if (info$byte_skip < 0) {
    # -1 means that the data are at the very end of the file. For raw data the position is computed
    # from the known size of the data, for a compressed stream the current position is the start of
    # the stream (it ends at the end of the file).
    if (encoding == "raw") {
      data_bytes <- num_values * type_info$itemsize;
      file_size <- file.size(filepath);
      skip <- file_size - data_bytes - data_offset;
      if (skip < 0) {
        stop(sprintf("File '%s' is truncated: %d bytes of data are expected at the end of the file, but the file is only %d bytes long.\n",
                     filepath, data_bytes, file_size));
      }
      if (!gzipped_file) {
        seek(con, where = data_offset + skip, origin = "start");
      } else {
        skip.connection.bytes(con, skip, gzipped = TRUE, filepath);
      }
    }
  } else if (info$byte_skip > 0) {
    skip.connection.bytes(con, info$byte_skip, gzipped = gzipped_file, filepath = filepath);
  }

  # The argument list of readBin is built explicitly: 'signed' is only accepted for the integer
  # types of 1 and 2 bytes, and the wide types are not read by readBin at all, see
  # nrrd.raw.to.numeric().
  read_values_directly <- function(con, num_values) {
    args <- list(con = con, what = type_info$what, n = num_values, size = type_info$size,
                 endian = info$endian);
    if (type_info$what == "integer" && type_info$size <= 2L) {
      args$signed <- type_info$signed;
    }
    return(do.call(readBin, args));
  };
  read_values_wide <- function(con, num_values) {
    return(nrrd.raw.to.numeric(readBin(con, what = "raw", n = num_values * type_info$itemsize),
                               type_info = type_info, endian = info$endian));
  };

  if (encoding == "raw") {
    values <- if (type_info$wide) {
      read_values_wide(con, num_values)
    } else {
      read_values_directly(con, num_values)
    };
  } else if (encoding == "ascii") {
    values <- suppressWarnings(scan(con, what = numeric(), n = num_values, quiet = TRUE));
  } else if (encoding == "gzip") {
    gz_con <- gzcon(con);
    values <- if (type_info$wide) {
      read_values_wide(gz_con, num_values)
    } else {
      read_values_directly(gz_con, num_values)
    };
    try(close(gz_con), silent = TRUE);
  } else {
    # bzip2: no streaming connection in R, so the compressed stream is read in one piece.
    decompressed <- memDecompress(nrrd.read.remainder(con), type = "bzip2");
    values <- if (type_info$wide) {
      nrrd.raw.to.numeric(readBin(decompressed, what = "raw", n = num_values * type_info$itemsize),
                          type_info = type_info, endian = info$endian)
    } else {
      args <- list(con = decompressed, what = type_info$what, n = num_values, size = type_info$size,
                   endian = info$endian);
      if (type_info$what == "integer" && type_info$size <= 2L) {
        args$signed <- type_info$signed;
      }
      do.call(readBin, args)
    };
  }

  if (num_values_exact && length(values) < num_values) {
    stop(sprintf("File '%s' is truncated or not in NRRD format: %d values were expected, %d could be read.\n",
                 filepath, num_values, length(values)));
  }
  return(values);
}


#' @title Read the data section of a NRRD file.
#'
#' @description Reads the values of a volume, from the header file itself or from the data files it
#'   names, and checks that their number matches the dimensions stated in the header.
#'
#' @param filepath character string, path to the header file.
#'
#' @param info named list, the parsed header, see \code{read.nrrd.header}.
#'
#' @param type_info named list, the R data type information, see \code{nrrd.type.info}.
#'
#' @return vector of values, of length \code{prod(sizes)}.
#'
#' @keywords internal
nrrd.read.data <- function(filepath, info, type_info) {
  num_values <- prod(as.numeric(info$sizes));

  if (is.null(info$data_files)) {
    values <- nrrd.read.values(filepath, info$data_offset, num_values, info = info, type_info = type_info);
  } else {
    # The LIST mode: the files hold consecutive parts of the data, in the order in which they are
    # listed. The exact number of values per file is not stated in the header, so the total number
    # of values of the whole volume is used as an upper bound for each file, and the concatenation
    # is checked against the header at the end.
    values <- numeric(0L);
    for (data_file in info$data_files) {
      part <- nrrd.read.values(data_file, 0, num_values, info = info, type_info = type_info,
                               num_values_exact = FALSE);
      values <- c(values, part);
    }
  }

  if (length(values) != num_values) {
    stop(sprintf("File '%s' is truncated or not in NRRD format: the header states %d values (%s of type '%s'), but %d could be read.\n",
                 filepath, num_values, paste(info$sizes, collapse = " x "), info$type, length(values)));
  }
  return(values);
}


#' @title Read the header of a file in NRRD format.
#'
#' @description Reads and parses the ASCII header of a NRRD file without touching the volume data.
#'   This is cheap, and it can be used to inspect a file, or to check its geometry, before deciding
#'   whether to read the data.
#'
#' @param filepath character string, path to the NRRD file (`.nrrd` or `.nhdr`).
#'
#' @return named list, the parsed header. The fields of the file are available under their
#'   normalized names (e.g. `space_directions` for the field 'space directions'), and fields that
#'   this function does not interpret are collected in the `other_fields` entry. The derived
#'   entries are `vox2ras_matrix` (the 4x4 matrix that maps 0-based voxel indices to RAS+
#'   coordinates, or NULL when the file states no geometry), `vox2ras_source` (how the matrix was
#'   derived: 'space directions' or 'spacings'), `data_files` (the resolved paths of the data files
#'   for a detached header, or NULL), `data_offset` (the byte offset of the data in the header
#'   file), `gzipped_file` (whether the whole file is gzip-compressed), `header_size` (the size of
#'   the header in bytes) and `dwi` (the parsed diffusion metadata, or NULL, see
#'   \code{\link{read.fs.volume.nrrd}}).
#'
#' @examples
#' nrrd_file <- system.file("extdata", "nrrd", "vol_u8_raw.nrrd",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' hdr <- read.nrrd.header(nrrd_file)
#' hdr$sizes
#' hdr$vox2ras_matrix
#'
#' @export
read.nrrd.header <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("Cannot read NRRD header, file '%s' does not exist or cannot be read.\n", filepath));
  }
  header <- nrrd.read.header.lines(filepath);
  parsed <- nrrd.parse.header(header$lines, filepath = filepath);
  fields <- parsed$fields;

  sizes <- nrrd.field(fields, "sizes");
  if (is.null(sizes) || length(sizes) == 0L || any(is.na(sizes)) || any(sizes <= 0L)) {
    stop(sprintf("NRRD file '%s' has no valid 'sizes' header field.\n", filepath));
  }
  dimension <- nrrd.field(fields, "dimension");
  if (is.null(dimension)) {
    dimension <- as.integer(length(sizes));
  }
  if (length(sizes) != dimension) {
    stop(sprintf("NRRD file '%s' is inconsistent: 'dimension' is %d but 'sizes' has %d entries.\n",
                 filepath, dimension, length(sizes)));
  }
  if (is.null(nrrd.field(fields, "encoding"))) {
    stop(sprintf("NRRD file '%s' has no 'encoding' header field.\n", filepath));
  }
  type_info <- nrrd.type.info(nrrd.field(fields, "type"), filepath = filepath);

  endian <- nrrd.field(fields, "endian");
  if (is.null(endian)) {
    endian <- "little";
  }
  endian <- tolower(trimws(endian));
  if (!endian %in% c("little", "big")) {
    stop(sprintf("Invalid NRRD 'endian' value '%s' in file '%s', expected 'little' or 'big'.\n",
                 endian, filepath));
  }

  geometry <- nrrd.vox2ras(fields, dimension = dimension, filepath = filepath);
  line_skip <- nrrd.field(fields, "line skip", 0L);
  byte_skip <- nrrd.field(fields, "byte skip", 0L);

  interpreted_keys <- c(
    "type", "dimension", "sizes", "encoding", "endian", "space", "spacedirections", "spaceorigin",
    "spacings", "kinds", "labels", "units", "spaceunits", "measurementframe", "datafile",
    "lineskip", "byteskip", "spacedimension", "centerings", "thicknesses", "axismins", "axismaxs",
    "content", "min", "max", "oldmin", "oldmax", "sampleunits"
  );
  dwi_keys <- grep("^dwmri", names(fields), value = TRUE);
  other_keys <- setdiff(names(fields), c(interpreted_keys, dwi_keys));
  other_fields <- fields[other_keys];
  names(other_fields) <- parsed$field_names[other_keys];

  result <- list(
    magic = parsed$magic,
    type = nrrd.field(fields, "type"),
    dimension = as.integer(dimension),
    sizes = as.integer(sizes),
    encoding = trimws(as.character(nrrd.field(fields, "encoding"))),
    endian = endian,
    space = nrrd.field(fields, "space"),
    space_directions = nrrd.field(fields, "space directions"),
    space_origin = nrrd.field(fields, "space origin"),
    spacings = nrrd.field(fields, "spacings"),
    kinds = nrrd.field(fields, "kinds"),
    labels = nrrd.field(fields, "labels"),
    units = nrrd.field(fields, "units"),
    measurement_frame = nrrd.field(fields, "measurement frame"),
    data_file = nrrd.field(fields, "data file"),
    line_skip = as.integer(line_skip),
    byte_skip = as.integer(byte_skip),
    data_offset = header$data_offset,
    data_files = nrrd.resolve.data.files(fields, parsed$data_file_names, filepath),
    gzipped_file = header$gzipped_file,
    header_size = header$header_bytes,
    header_lines = header$lines,
    vox2ras_matrix = geometry$matrix,
    vox2ras_source = geometry$source,
    type_info = type_info,
    other_fields = other_fields,
    dwi = nrrd.dwi.info(fields, num_volumes = if (dimension >= 4L) sizes[4L] else NA_integer_),
    filepath = filepath
  );
  return(result);
}


#' @title Read volume data from a file in NRRD format.
#'
#' @description Reads a volume from a file in NRRD format (`.nrrd`), i.e. the format that 3D Slicer,
#'   ITK/SimpleITK, DTI-TK, DSI Studio and the `teem` tools use. Detached headers (`.nhdr`, whose
#'   data are in separate files) and gzip-compressed files (`.nrrd.gz`) are supported as well, and
#'   the reader is implemented in this package, so no additional R package or external software is
#'   needed. The values are returned in the order in which they are stored in the file, and NRRD
#'   stores the first axis fastest, which is the order that R uses for arrays as well, so the
#'   returned array is shaped exactly like the volume described by the header.
#'
#' @param filepath character string, path to the file in NRRD format.
#'
#' @param flatten logical, whether to return a numeric vector instead of a multidimensional array.
#'
#' @param with_header logical, whether to return an `fs.volume` instance (a named list with the
#'   entries `data` and `header`) instead of the data array. The header contains the geometry of
#'   the volume in the `vox2ras_matrix` entry, see \code{\link{read.nrrd.header}}.
#'
#' @param drop_empty_dims logical, whether to drop dimensions of length 1 from the returned data,
#'   e.g. the frame axis of a 3D volume that was stored as a 4D array with one volume. Note that
#'   NRRD files state their dimensionality explicitly, so unlike the MGH or NIfTI readers this one
#'   only drops dimensions that the file actually has.
#'
#' @return a multidimensional array of the dimensions stated in the file header, a numeric vector
#'   if `flatten` is `TRUE`, or an `fs.volume` instance if `with_header` is `TRUE`. The data type
#'   of the array follows the file: integer for the integer types up to 32 bit, double for the
#'   floating point types and for the 64 bit integer types (which have no R equivalent, values
#'   above 2^53 lose precision, and a warning is raised in that case).
#'
#' @note The data are returned exactly as they are stored in the file: the reader does not
#'   reorient, rescale or otherwise change them. Use the `vox2ras_matrix` entry of the header to
#'   map voxel indices to RAS+ coordinates, which is the space in which FreeSurfer, NIfTI and the
#'   rest of this package work. NRRD files that state the LPS space (which is what the ITK tools
#'   write) are handled: the matrix is converted to RAS. For a volume whose file states no space
#'   information at all, no matrix is available (the entry is NULL), since any matrix would be a
#'   guess.
#'
#' @note Diffusion MRI data in NRRD format carry their b-value and their gradient directions in
#'   custom header fields (the convention of the `teem` tools, DTI-TK and 3D Slicer). They are
#'   parsed into the `dwi` entry of the header by \code{\link{read.nrrd.header}}. The gradients are
#'   given in the image frame, and the `measurement_frame` entry of the `dwi` list is the rotation
#'   that maps them into the frame in which the measurement was performed; a caller that needs the
#'   gradients in that frame has to apply it (multiply the vector by the matrix). The values can be
#'   handed to \code{\link{read.dti.gradients}} together with one b-value per volume, see the
#'   examples.
#'
#' @examples
#' nrrd_file <- system.file("extdata", "nrrd", "vol_u8_raw.nrrd",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' vol <- read.fs.volume.nrrd(nrrd_file)
#' dim(vol)
#'
#' # Read the geometry as well:
#' volh <- read.fs.volume.nrrd(nrrd_file, with_header = TRUE)
#' volh$header$vox2ras_matrix
#'
#' \dontrun{
#' # Read a DWI volume and use its gradient table:
#' hdr <- read.nrrd.header("dwi.nrrd")
#' gradients <- read.dti.gradients(hdr$dwi$bvec, rep(hdr$dwi$b_value, hdr$dwi$num_gradients))
#' }
#'
#' @export
read.fs.volume.nrrd <- function(filepath, flatten = FALSE, with_header = FALSE, drop_empty_dims = FALSE) {
  if (!file.exists(filepath)) {
    stop(sprintf("Cannot read volume, file '%s' does not exist or cannot be read.\n", filepath));
  }

  header <- read.nrrd.header(filepath);
  type_info <- header$type_info;

  # Refuse an implausible allocation before reading anything, like the other volume readers do.
  validate_allocation_size(header$sizes, type_info$itemsize,
                           label = sprintf("the voxel data of NRRD file '%s'", filepath));

  values <- nrrd.read.data(filepath, info = header, type_info = type_info);

  if (type_info$is_int64 && length(values) > 0L) {
    max_abs <- max(abs(values));
    if (is.finite(max_abs) && max_abs > 2^53) {
      warning(sprintf("NRRD file '%s' stores 64 bit integer values, but R has no 64 bit integer type, so the values are returned as doubles. The largest value in the file (%.0f) is larger than 2^53, so it cannot be represented exactly.\n",
                      filepath, max_abs));
    }
  }

  data <- array(values, dim = header$sizes);
  if (drop_empty_dims) {
    # Only drop dimensions that the file actually states, see the doc of the parameter.
    drop_dims <- which(header$sizes == 1L);
    if (length(drop_dims) > 0L) {
      data <- array(values, dim = header$sizes[-drop_dims]);
    }
  }
  if (flatten) {
    dim(data) <- c(length(values));
    data <- as.vector(unlist(data));
    header$voldim <- c(length(data));
  }

  if (with_header) {
    return_list <- list();
    return_list$header <- header;
    return_list$data <- data;
    class(return_list) <- "fs.volume";
    return(return_list);
  }
  return(data);
}
