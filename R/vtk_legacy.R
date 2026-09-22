# Support for the legacy VTK file format (meshes, point clouds and tracts) -----
#
# VTK stores data either in the "legacy" format (a text file whose first line
# reads '# vtk DataFile Version x.y', optionally followed by binary payloads) or
# in one of the XML based formats (.vtp, .vti, ...). This file implements reading
# of the legacy format for datasets of type POLYDATA, which is the dataset type
# used for meshes, point clouds and streamlines (tracts).
#
# Two properties of the format make a careful implementation necessary:
#
# 1. The encoding. The payload is either ASCII (numbers separated by whitespace,
#    with no guarantee about how many of them share a line) or BINARY (raw bytes
#    in *big endian* order -- the legacy format has no means of recording the
#    byte order and VTK always writes network byte order, i.e. big endian).
#
# 2. The layout of the cell arrays (VERTICES, LINES, POLYGONS,
#    TRIANGLE_STRIPS). Up to VTK 4.2 the cell array was a single sequence of
#    integers in which every cell was represented by its vertex count followed by
#    the vertex indices, so 'POLYGONS 12 48' announced 12 cells occupying 48
#    integers. Since VTK 5.1 (2015) the cell array is split into an OFFSETS and a
#    CONNECTIVITY array which are preceded by their own keywords and type names,
#    and the two numbers in the section header line are (number of values in the
#    offsets array, number of values in the connectivity array) instead. That
#    change applies to ASCII and binary files alike, and software still actively
#    writes both layouts, so a reader has to support both. Which one is present
#    cannot be derived from the version number in the header (it is not even
#    consistent between files written by the same VTK version), it has to be
#    detected from the presence of the OFFSETS keyword, which is what this
#    implementation does.
#
# Only geometry is read: attribute data (NORMALS, TEXTURE_COORDINATES, SCALARS,
# ...) and everything that follows it is ignored. That is safe because VTK always
# writes the geometry arrays (POINTS, VERTICES, LINES, POLYGONS) before the
# attribute arrays.

# The sizes of the types as used by the VTK legacy format. 'long' and
# 'unsigned_long' are deliberately absent: their size depends on the platform
# the file was written on and the format does not record it, so guessing would
# be wrong for files that were written on a different platform. 'vtkidtype' is
# absent for the same reason, its size depends on how VTK was built.
vtk.data.types <- list(
  "char" = list(size = 1L, mode = "integer", integer = TRUE),
  "signed_char" = list(size = 1L, mode = "integer", integer = TRUE),
  "unsigned_char" = list(size = 1L, mode = "integer", integer = TRUE),
  "short" = list(size = 2L, mode = "integer", integer = TRUE),
  "unsigned_short" = list(size = 2L, mode = "integer", integer = TRUE),
  "int" = list(size = 4L, mode = "integer", integer = TRUE),
  "unsigned_int" = list(size = 4L, mode = "integer", integer = TRUE),
  "long_long" = list(size = 8L, mode = "integer", integer = TRUE),
  "unsigned_long_long" = list(size = 8L, mode = "integer", integer = TRUE),
  "float" = list(size = 4L, mode = "double", integer = FALSE),
  "double" = list(size = 8L, mode = "double", integer = FALSE),
  "vtktypeint8" = list(size = 1L, mode = "integer", integer = TRUE),
  "vtktypeuint8" = list(size = 1L, mode = "integer", integer = TRUE),
  "vtktypeint16" = list(size = 2L, mode = "integer", integer = TRUE),
  "vtktypeuint16" = list(size = 2L, mode = "integer", integer = TRUE),
  "vtktypeint32" = list(size = 4L, mode = "integer", integer = TRUE),
  "vtktypeuint32" = list(size = 4L, mode = "integer", integer = TRUE),
  "vtktypeint64" = list(size = 8L, mode = "integer", integer = TRUE),
  "vtktypeuint64" = list(size = 8L, mode = "integer", integer = TRUE),
  "vtktypefloat32" = list(size = 4L, mode = "double", integer = FALSE),
  "vtktypefloat64" = list(size = 8L, mode = "double", integer = FALSE)
)

# The cell array section keywords of a POLYDATA dataset. 'TRIANGLE_STRIPS' is
# recognized so that it can be rejected with a helpful message instead of being
# mistaken for an unknown section.
vtk.cell.section.keywords <- c("VERTICES", "LINES", "POLYGONS", "TRIANGLE_STRIPS")

# Sections that are not geometry and are therefore skipped silently. Everything
# after such a section is ignored as well, see the comment at the top of this
# file.
vtk.attribute.section.keywords <- c(
  "POINT_DATA", "CELL_DATA", "SCALARS", "COLOR_SCALARS", "LOOKUP_TABLE",
  "VECTORS", "NORMALS", "TENSORS", "TEXTURE_COORDINATES", "FIELD",
  "GLOBAL_IDS", "PEDIGREE_IDS", "EDGE_FLAGS", "METADATA", "CURVATURE"
)


#' @title Get the on-disk properties of a VTK legacy data type.
#'
#' @description Every data section of a VTK legacy file names the type of its
#'   values explicitly (e.g. \code{POINTS 8 float}). This function translates
#'   such a type name into the number of bytes a single value occupies on disk,
#'   the R storage mode to read it into, and whether the values are integers.
#'
#' @param type_name character string, the type name as it occurs in the file,
#'   e.g. 'float', 'double' or 'vtktypeint64'.
#'
#' @param filepath character string, the file the type name was found in. Only
#'   used to make error messages more helpful.
#'
#' @return named list with the entries 'size' (integer, bytes per value on disk),
#'   'mode' (character, the storage mode for \code{\link[base]{readBin}}) and
#'   'integer' (logical, whether the values are integers).
#'
#' @keywords internal
vtk.data.type.info <- function(type_name, filepath = "") {
  info <- vtk.data.types[[tolower(trimws(type_name))]]
  if (is.null(info)) {
    stop(sprintf(
      "Unsupported VTK data type '%s' in file '%s'. Supported types are: %s.\n",
      type_name, filepath, paste(names(vtk.data.types), collapse = ", ")
    ))
  }
  return(info)
}


#' @title Create the low-level state used while reading a VTK legacy file.
#'
#' @description Opens a connection to the file (transparently handling gzip
#'   compression) and initializes the lookahead buffer. The returned environment
#'   is passed to the other \code{vtk.*} functions, which mutate its state while
#'   they consume the file.
#'
#' @param filepath character string, path to the VTK file.
#'
#' @return an environment with the class 'vtk.reader'.
#'
#' @keywords internal
vtk.reader.new <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("File '%s' does not exist or is not readable.\n", filepath))
  }
  gzipped <- is.gzip.file(filepath)
  reader <- new.env(parent = emptyenv())
  reader$filepath <- filepath
  reader$gzipped <- gzipped
  reader$con <- open.maybe.gzip(filepath, gzipped, mode = "rb")
  # Bytes that were read from the connection but not yet consumed. This is what
  # allows peeking at the next byte without losing it, which is required to
  # detect the cell array layout of binary files.
  reader$buffer <- raw(0L)
  # State for the ASCII encoding, unused for binary files.
  reader$encoding <- NA_character_
  reader$lines <- character(0L)
  reader$line_idx <- 1L
  reader$pending_line <- NULL
  return(reader)
}


#' @title Close the connection of a VTK legacy reader.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return \code{NULL}, invisibly.
#'
#' @keywords internal
vtk.reader.close <- function(reader) {
  if (!is.null(reader$con)) {
    close(reader$con)
    reader$con <- NULL
  }
  return(invisible(NULL))
}


#' @title Fill the lookahead buffer of a VTK legacy reader.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_bytes single non-negative integer, the number of bytes the buffer
#'   should hold.
#'
#' @return \code{NULL}, invisibly. The buffer may hold fewer bytes than requested
#'   if the file ends.
#'
#' @keywords internal
vtk.reader.fill <- function(reader, num_bytes) {
  missing_bytes <- num_bytes - length(reader$buffer)
  if (missing_bytes > 0) {
    more <- readBin(reader$con, what = "raw", n = missing_bytes)
    if (length(more) > 0L) {
      reader$buffer <- c(reader$buffer, more)
    }
  }
  return(invisible(NULL))
}


#' @title Read bytes from a VTK legacy reader without consuming them.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_bytes single non-negative integer, the number of bytes to peek at.
#'
#' @return raw vector, possibly shorter than \code{num_bytes} at the end of the file.
#'
#' @keywords internal
vtk.reader.peek <- function(reader, num_bytes) {
  vtk.reader.fill(reader, num_bytes)
  return(reader$buffer[seq_len(min(num_bytes, length(reader$buffer)))])
}


#' @title Consume bytes from a VTK legacy reader.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_bytes single non-negative integer, the number of bytes to consume.
#'
#' @return raw vector of length \code{num_bytes}.
#'
#' @keywords internal
vtk.reader.bytes <- function(reader, num_bytes) {
  vtk.reader.fill(reader, num_bytes)
  if (length(reader$buffer) < num_bytes) {
    stop(sprintf(
      "File '%s' is truncated: expected %d more byte(s) but the file ends.\n",
      reader$filepath, num_bytes
    ))
  }
  out <- reader$buffer[seq_len(num_bytes)]
  reader$buffer <- if (length(reader$buffer) > num_bytes) {
    reader$buffer[(num_bytes + 1L):length(reader$buffer)]
  } else {
    raw(0L)
  }
  return(out)
}


#' @title Read numeric values from a binary VTK legacy file.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_values single non-negative integer, the number of values to read.
#'
#' @param type_info named list, the result of \code{\link{vtk.data.type.info}}.
#'
#' @return numeric or integer vector of length \code{num_values}.
#'
#' @note Legacy VTK binary data is always big endian, the format has no way of
#'   expressing a different byte order.
#'
#' @keywords internal
vtk.reader.numbers <- function(reader, num_values, type_info) {
  if (num_values == 0L) {
    return(if (isTRUE(type_info$integer)) integer(0L) else numeric(0L))
  }

  if (length(reader$buffer) == 0L) {
    # Fast path: nothing was peeked at, so the values can be read straight from
    # the connection. This matters for the POINTS section, which is by far the
    # largest part of a mesh file.
    values <- readBin(reader$con,
      what = type_info$mode, n = num_values,
      size = type_info$size, endian = "big"
    )
    if (length(values) != num_values) {
      stop(sprintf(
        "File '%s' is truncated: expected %d value(s) but only %d are present.\n",
        reader$filepath, num_values, length(values)
      ))
    }
    return(values)
  }

  # Slow path: the lookahead buffer holds bytes that belong to this section, so
  # they have to be consumed together with the remaining bytes.
  num_bytes <- num_values * type_info$size
  raw_bytes <- c(reader$buffer, readBin(reader$con, what = "raw", n = num_bytes - length(reader$buffer)))
  reader$buffer <- raw(0L)
  if (length(raw_bytes) != num_bytes) {
    stop(sprintf(
      "File '%s' is truncated: expected %d more byte(s) but the file ends.\n",
      reader$filepath, num_bytes - length(raw_bytes)
    ))
  }
  con <- rawConnection(raw_bytes, open = "rb")
  on.exit(
    {
      close(con)
    },
    add = TRUE
  )
  return(readBin(con, what = type_info$mode, n = num_values, size = type_info$size, endian = "big"))
}


#' @title Read the next line of an ASCII VTK legacy file.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return character string, or \code{NULL} at the end of the file.
#'
#' @keywords internal
vtk.next.line <- function(reader) {
  if (!is.null(reader$pending_line)) {
    line <- reader$pending_line
    reader$pending_line <- NULL
    return(line)
  }
  if (reader$line_idx > length(reader$lines)) {
    return(NULL)
  }
  line <- reader$lines[reader$line_idx]
  reader$line_idx <- reader$line_idx + 1L
  return(line)
}


#' @title Look at the next line of an ASCII VTK legacy file.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return character string, or \code{NULL} at the end of the file. The line is
#'   not consumed, the next call to \code{\link{vtk.next.line}} returns it.
#'
#' @keywords internal
vtk.peek.line <- function(reader) {
  if (is.null(reader$pending_line)) {
    if (reader$line_idx > length(reader$lines)) {
      return(NULL)
    }
    reader$pending_line <- reader$lines[reader$line_idx]
    reader$line_idx <- reader$line_idx + 1L
  }
  return(reader$pending_line)
}


#' @title Split a line of a VTK legacy file into whitespace separated tokens.
#'
#' @param line character string, the line to split.
#'
#' @return character vector, the tokens of the line.
#'
#' @keywords internal
vtk.split.line <- function(line) {
  return(strsplit(trimws(line), "[ \t]+")[[1L]])
}


#' @title Read the header of a VTK legacy file and set up the reader.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return named list with the entries 'version' (character), 'encoding' ('ASCII'
#'   or 'BINARY') and 'dataset' (character, the dataset type).
#'
#' @keywords internal
vtk.read.header <- function(reader) {
  filepath <- reader$filepath
  header_lines <- readLines(reader$con, n = 4L, warn = FALSE)
  if (length(header_lines) < 4L) {
    stop(sprintf("File '%s' is not a valid VTK legacy file: it does not contain the 4 header lines.\n", filepath))
  }
  if (!startsWith(header_lines[1L], "# vtk DataFile Version")) {
    stop(sprintf("File '%s' is not a valid VTK legacy file: the first line does not read '# vtk DataFile Version'.\n", filepath))
  }
  version <- trimws(sub("# vtk DataFile Version", "", header_lines[1L], fixed = TRUE))

  encoding <- toupper(trimws(header_lines[3L]))
  if (!(encoding %in% c("ASCII", "BINARY"))) {
    stop(sprintf(
      "File '%s' is not a valid VTK legacy file: the third header line must read 'ASCII' or 'BINARY', but it reads '%s'.\n",
      filepath, trimws(header_lines[3L])
    ))
  }

  dataset_line <- trimws(header_lines[4L])
  if (!startsWith(dataset_line, "DATASET")) {
    stop(sprintf("File '%s' is not a valid VTK legacy file: the fourth header line does not start with 'DATASET'.\n", filepath))
  }
  dataset <- trimws(sub("DATASET", "", dataset_line, fixed = TRUE))
  if (dataset != "POLYDATA") {
    stop(sprintf(
      "File '%s' contains a VTK dataset of type '%s', but only 'POLYDATA' datasets (meshes, point clouds, tracts) are supported.\n",
      filepath, dataset
    ))
  }

  reader$encoding <- encoding
  if (encoding == "ASCII") {
    # For ASCII files all remaining lines are read at once and empty lines are
    # dropped, which makes the section parser considerably simpler.
    lines <- readLines(reader$con, warn = FALSE)
    lines <- trimws(lines)
    reader$lines <- lines[nzchar(lines)]
    reader$line_idx <- 1L
  }

  return(list(version = version, encoding = encoding, dataset = dataset))
}


#' @title Read the next section header line of a VTK legacy file.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return named list with the entries 'keyword' (character, e.g. 'POLYGONS') and
#'   'args' (character vector, the remaining tokens of the line), or \code{NULL}
#'   at the end of the file.
#'
#' @keywords internal
vtk.next.section <- function(reader) {
  if (reader$encoding == "ASCII") {
    line <- vtk.next.line(reader)
    if (is.null(line)) {
      return(NULL)
    }
  } else {
    # Binary files use newlines only as separators around the section header
    # lines (the payload itself is raw bytes), and VTK writes one after every
    # data block, so empty lines have to be skipped here.
    repeat {
      if (length(reader$buffer) > 0L) {
        stop("Internal error: unread bytes in the lookahead buffer of a binary VTK file.\n") # nocov
      }
      line <- readLines(reader$con, n = 1L, warn = FALSE)
      if (length(line) == 0L) {
        return(NULL)
      }
      line <- trimws(line[1L])
      if (nzchar(line)) {
        break
      }
    }
  }

  tokens <- vtk.split.line(line)
  if (!grepl("^[A-Z][A-Z0-9_]*$", tokens[1L])) {
    stop(sprintf(
      "File '%s' is not a valid VTK legacy file: expected a section header line, but found '%s'.\n",
      reader$filepath, line
    ))
  }
  return(list(keyword = tokens[1L], args = tokens[-1L]))
}


#' @title Read a non-negative integer from a VTK section header.
#'
#' @param value character string, the token to parse.
#'
#' @param filepath character string, the file being read, for error messages.
#'
#' @param what character string, a description of the value, for error messages.
#'
#' @return single integer.
#'
#' @keywords internal
vtk.parse.count <- function(value, filepath, what) {
  count <- suppressWarnings(as.numeric(value))
  if (length(count) != 1L || is.na(count) || count < 0 || count != floor(count) || count > .Machine$integer.max) {
    stop(sprintf("Invalid %s '%s' in a section header of VTK file '%s'.\n", what, value, filepath))
  }
  return(as.integer(count))
}


#' @title Read numeric values from a VTK legacy section.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_values single non-negative integer, the number of values to read.
#'
#' @param type_info named list, the result of \code{\link{vtk.data.type.info}}.
#'
#' @return numeric or integer vector of length \code{num_values}.
#'
#' @keywords internal
vtk.section.values <- function(reader, num_values, type_info) {
  if (reader$encoding == "ASCII") {
    return(vtk.section.values.ascii(reader, num_values))
  }
  return(vtk.reader.numbers(reader, num_values, type_info))
}


#' @title Read numeric values from an ASCII VTK legacy section.
#'
#' @description The ASCII encoding does not guarantee how many values share a
#'   line, so values are collected line by line until the expected number of
#'   them has been read. A line that contains more values than the section
#'   declares is treated as an error, because it means that the section header
#'   lied about the size of the section and the file cannot be parsed reliably.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param num_values single non-negative integer, the number of values to read.
#'
#' @return numeric vector of length \code{num_values}.
#'
#' @keywords internal
vtk.section.values.ascii <- function(reader, num_values) {
  if (num_values == 0L) {
    return(numeric(0L))
  }
  chunks <- vector("list", 16L) # grown as needed, usually only a few are used
  num_chunks <- 0L
  num_collected <- 0L
  while (num_collected < num_values) {
    line <- vtk.next.line(reader)
    if (is.null(line)) {
      stop(sprintf(
        "File '%s' is truncated: expected %d more value(s) but the file ends.\n",
        reader$filepath, num_values - num_collected
      ))
    }
    tokens <- vtk.split.line(line)
    values <- suppressWarnings(as.numeric(tokens))
    if (any(is.na(values) & !grepl("^[-+]?nan$", tokens, ignore.case = TRUE))) {
      if (grepl("^[A-Z][A-Z0-9_]*$", tokens[1L])) {
        # A section header line, so the section that is being read here ends before it announced to.
        stop(sprintf(
          "File '%s' is truncated: a section announces %d value(s) but only %d are present before the next section starts.\n",
          reader$filepath, num_values, num_collected
        ))
      }
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: expected numbers, but found '%s'.\n",
        reader$filepath, line
      ))
    }
    if (num_collected + length(values) > num_values) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: a section declares %d value(s) but a line contains more than that.\n",
        reader$filepath, num_values
      ))
    }
    num_chunks <- num_chunks + 1L
    if (num_chunks > length(chunks)) {
      chunks[[2L * length(chunks)]] <- numeric(0L)
    }
    chunks[[num_chunks]] <- values
    num_collected <- num_collected + length(values)
  }
  if (num_chunks == 1L) {
    return(chunks[[1L]])
  }
  return(unlist(chunks[seq_len(num_chunks)], use.names = FALSE))
}


#' @title Convert values that are used as indices to integers.
#'
#' @param values numeric vector, the values to convert.
#'
#' @param filepath character string, the file the values were read from.
#'
#' @return integer vector.
#'
#' @keywords internal
vtk.as.indices <- function(values, filepath) {
  if (any(is.na(values)) || any(values < 0) || any(values > .Machine$integer.max)) {
    stop(sprintf(
      "File '%s' contains invalid vertex indices: the values must be non-negative integers that fit into a 32 bit integer.\n",
      filepath
    ))
  }
  return(as.integer(values))
}


#' @title Determine how the cell array of a section is stored.
#'
#' @description The cell arrays of VTK legacy files come in two layouts, see the
#'   comment at the top of this file. Both layouts are still written by software
#'   in use today, and the version number in the header does not reliably
#'   indicate which one a file uses, so the layout is detected from the presence
#'   of the OFFSETS keyword: the old layout starts the cell data with the vertex
#'   count of the first cell as a raw number, whereas the new layout starts it
#'   with the string 'OFFSETS'. For ASCII files that is simply the next line; for
#'   binary files the first byte decides, and because a cell vertex count is a
#'   small integer, the first byte of an old style cell array is always zero.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @return named list with the entry 'layout' ('old' or 'new') and, for the new
#'   layout, the entry 'type' (character, the name of the offsets data type).
#'
#' @keywords internal
vtk.cell.section.layout <- function(reader) {
  filepath <- reader$filepath

  if (reader$encoding == "ASCII") {
    line <- vtk.peek.line(reader)
    if (is.null(line)) {
      stop(sprintf("File '%s' is truncated: the cell array data is missing.\n", filepath))
    }
    tokens <- vtk.split.line(line)
    if (!identical(tokens[1L], "OFFSETS")) {
      return(list(layout = "old", type = NA_character_))
    }
    reader$pending_line <- NULL # consume the line
    if (length(tokens) < 2L) {
      stop(sprintf("File '%s' is not a valid VTK legacy file: the OFFSETS section header is missing the data type.\n", filepath))
    }
    return(list(layout = "new", type = tokens[2L]))
  }

  first_byte <- vtk.reader.peek(reader, 1L)
  if (length(first_byte) == 0L) {
    stop(sprintf("File '%s' is truncated: the cell array data is missing.\n", filepath))
  }
  byte_value <- as.integer(first_byte[1L])
  if (byte_value < 65L || byte_value > 90L) { # not an uppercase ASCII letter
    return(list(layout = "old", type = NA_character_))
  }

  consumed <- vtk.reader.bytes(reader, 1L)
  rest_of_line <- readLines(reader$con, n = 1L, warn = FALSE)
  if (length(rest_of_line) == 0L) {
    stop(sprintf("File '%s' is truncated: the cell array data is missing.\n", filepath))
  }
  tokens <- vtk.split.line(paste0(rawToChar(consumed), rest_of_line[1L]))
  if (!identical(tokens[1L], "OFFSETS")) {
    stop(sprintf(
      "File '%s' is not a supported VTK legacy file: expected 'OFFSETS' at the start of a cell array, but found '%s'.\n",
      filepath, tokens[1L]
    ))
  }
  if (length(tokens) < 2L) {
    stop(sprintf("File '%s' is not a valid VTK legacy file: the OFFSETS section header is missing the data type.\n", filepath))
  }
  return(list(layout = "new", type = tokens[2L]))
}


#' @title Read the cell array of a VTK legacy section.
#'
#' @param reader an environment as returned by \code{\link{vtk.reader.new}}.
#'
#' @param section named list, a section as returned by \code{\link{vtk.next.section}}.
#'
#' @return list of integer vectors, one per cell, containing the 0-based vertex
#'   indices of the cell.
#'
#' @keywords internal
vtk.parse.cell.section <- function(reader, section) {
  filepath <- reader$filepath
  keyword <- section$keyword

  if (keyword == "TRIANGLE_STRIPS") {
    stop(sprintf(
      "File '%s' contains a TRIANGLE_STRIPS section, which is not supported. Convert the mesh to triangles first.\n",
      filepath
    ))
  }

  if (length(section$args) < 2L) {
    stop(sprintf(
      "File '%s' is not a valid VTK legacy file: the '%s' section header must contain two numbers.\n",
      filepath, keyword
    ))
  }
  header_first <- vtk.parse.count(section$args[1L], filepath, "cell count")
  header_second <- vtk.parse.count(section$args[2L], filepath, "cell array size")

  # The number of cells is taken from the file, so it has to be checked before a list of that size is
  # allocated. It is at most the first number of the section header, in both layouts. The 128 bytes per cell
  # are an estimate: every cell is stored as a separate integer vector of a few elements, which costs
  # considerably more than the vertex indices it holds.
  validate_allocation_size(header_first, bytes_per_elem = 128, label = "the VTK cells")

  layout <- vtk.cell.section.layout(reader)

  if (layout$layout == "new") {
    offsets <- vtk.section.values(reader, header_first, vtk.data.type.info(layout$type, filepath))
    num_cells <- header_first - 1L
    if (num_cells < 0L) {
      stop(sprintf("File '%s' is not a valid VTK legacy file: the offsets array of the '%s' section is empty.\n", filepath, keyword))
    }
    offsets <- vtk.as.indices(offsets, filepath)
    if (length(offsets) != num_cells + 1L || offsets[1L] != 0L || is.unsorted(offsets)) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: the offsets array of the '%s' section must start at 0 and be sorted.\n",
        filepath, keyword
      ))
    }

    connectivity_section <- vtk.next.section(reader)
    if (is.null(connectivity_section) || !identical(connectivity_section$keyword, "CONNECTIVITY")) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: the OFFSETS section of the '%s' cell array is not followed by a CONNECTIVITY section.\n",
        filepath, keyword
      ))
    }
    connectivity <- vtk.section.values(reader, header_second, vtk.data.type.info(connectivity_section$args[1L], filepath))
    connectivity <- vtk.as.indices(connectivity, filepath)
    if (offsets[num_cells + 1L] != length(connectivity)) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: the last offset of the '%s' section is %d, but the connectivity array has %d value(s).\n",
        filepath, keyword, offsets[num_cells + 1L], length(connectivity)
      ))
    }

    cells <- vector("list", num_cells)
    for (cell_idx in seq_len(num_cells)) {
      if (offsets[cell_idx + 1L] > offsets[cell_idx]) {
        cells[[cell_idx]] <- connectivity[(offsets[cell_idx] + 1L):offsets[cell_idx + 1L]]
      } else {
        cells[[cell_idx]] <- integer(0L)
      }
    }
    return(cells)
  }

  # Old layout: the cell array is a single sequence of integers in which every
  # cell is represented by its vertex count followed by the vertex indices. The
  # legacy format does not name the type of these values; VTK writes them as 4
  # byte integers, which is what the binary reader assumes.
  values <- vtk.section.values(reader, header_second, vtk.data.types[["int"]])
  values <- vtk.as.indices(values, filepath)

  cells <- vector("list", header_first)
  num_cells <- 0L
  pos <- 1L
  num_values <- length(values)
  while (pos <= num_values) {
    num_cell_vertices <- values[pos]
    if (num_cell_vertices < 1L || pos + num_cell_vertices > num_values) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: the '%s' cell array is malformed.\n",
        filepath, keyword
      ))
    }
    if (num_cells >= header_first) {
      stop(sprintf(
        "File '%s' is not a valid VTK legacy file: the '%s' section contains more cells than the %d announced in its header.\n",
        filepath, keyword, header_first
      ))
    }
    num_cells <- num_cells + 1L
    cells[[num_cells]] <- values[(pos + 1L):(pos + num_cell_vertices)]
    pos <- pos + num_cell_vertices + 1L
  }
  if (num_cells != header_first) {
    stop(sprintf(
      "File '%s' is not a valid VTK legacy file: the '%s' section header announces %d cell(s), but the data contains %d.\n",
      filepath, keyword, header_first, num_cells
    ))
  }
  return(cells)
}


#' @title Read a VTK legacy file with a POLYDATA dataset.
#'
#' @description Reads the geometry (points and the cell arrays VERTICES, LINES
#'   and POLYGONS) of a VTK legacy file. Both the ASCII and the binary encoding
#'   are supported, as are the old and the new cell array layout, see the comment
#'   at the top of the file. Attribute data such as normals, texture coordinates
#'   or scalars is ignored.
#'
#' @param filepath character string, path to the VTK file.
#'
#' @return named list with the entries 'version' (character, the VTK version from
#'   the header), 'encoding' (character, 'ASCII' or 'BINARY'), 'points' (n x 3
#'   double matrix, or NULL), 'verts', 'lines' and 'polys' (each a list of
#'   integer vectors with 0-based vertex indices, or NULL if the file does not
#'   contain that cell type), and 'ignored_sections' (character vector with the
#'   keywords of the sections that were not read, e.g. 'POINT_DATA').
#'
#' @keywords internal
read.vtk.legacy.polydata <- function(filepath) {
  reader <- vtk.reader.new(filepath)
  on.exit(
    {
      vtk.reader.close(reader)
    },
    add = TRUE
  )

  header <- vtk.read.header(reader)

  points <- NULL
  verts <- NULL
  lines <- NULL
  polys <- NULL
  ignored_sections <- character(0L)

  repeat {
    section <- vtk.next.section(reader)
    if (is.null(section)) {
      break
    }
    keyword <- section$keyword

    if (keyword == "POINTS") {
      if (!is.null(points)) {
        stop(sprintf("File '%s' is not a valid VTK legacy file: it contains more than one POINTS section.\n", filepath))
      }
      if (length(section$args) < 2L) {
        stop(sprintf("File '%s' is not a valid VTK legacy file: the POINTS section header is incomplete.\n", filepath))
      }
      num_points <- vtk.parse.count(section$args[1L], filepath, "point count")
      type_info <- vtk.data.type.info(section$args[2L], filepath)
      validate_allocation_size(num_points * 3, bytes_per_elem = 8, label = "the VTK points")
      values <- vtk.section.values(reader, num_points * 3L, type_info)
      points <- matrix(as.numeric(values), ncol = 3L, byrow = TRUE)
    } else if (keyword %in% vtk.cell.section.keywords) {
      cells <- vtk.parse.cell.section(reader, section)
      if (keyword == "VERTICES") {
        verts <- cells
      } else if (keyword == "LINES") {
        lines <- cells
      } else { # POLYGONS
        polys <- cells
      }
    } else {
      ignored_sections <- c(ignored_sections, keyword)
      if (!(keyword %in% vtk.attribute.section.keywords)) {
        warning(sprintf(
          "File '%s' contains a section with the unsupported keyword '%s', which was ignored.\n",
          filepath, keyword
        ))
      }
      # Attribute sections (and anything unrecognized) come after the geometry,
      # so there is nothing left to read.
      break
    }
  }

  if (is.null(points)) {
    stop(sprintf("File '%s' does not contain a POINTS section, so it contains no geometry.\n", filepath))
  }

  return(list(
    version = header$version, encoding = header$encoding,
    points = points, verts = verts, lines = lines, polys = polys,
    ignored_sections = ignored_sections
  ))
}


#' @title Check that all cells of the requested type are triangles.
#'
#' @param cells list of integer vectors, as returned by \code{\link{vtk.parse.cell.section}}.
#'
#' @param filepath character string, the file the cells were read from.
#'
#' @return \code{NULL}, invisibly. Stops if a cell is not a triangle.
#'
#' @keywords internal
vtk.check.triangles <- function(cells, filepath) {
  num_vertices <- vapply(cells, length, integer(1L))
  if (length(num_vertices) > 0L && any(num_vertices != 3L)) {
    stop(sprintf(
      "File '%s' contains polygons which are not triangles. Only triangular meshes are supported by this function. You can convert a quad mesh to triangles with 'faces.quad.to.tris'.\n",
      filepath
    ))
  }
  return(invisible(NULL))
}


#' @title Read VTK legacy format mesh as surface.
#'
#' @description Reads meshes from files in the VTK legacy format. Both the ASCII
#'   and the binary encoding are supported, as are the cell array layouts written
#'   by VTK 4.2 and older and by VTK 5.1 and newer, see the notes. See
#'   \url{https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf} for the
#'   format specification. Note that this function does **not** read arbitrary
#'   VTK datasets, it supports only the geometry of POLYDATA datasets (meshes and
#'   point clouds); attribute data such as normals, texture coordinates or
#'   scalars is ignored. Only triangular meshes are supported, files containing
#'   other polygons are rejected with an error.
#'
#' @param filepath string. Full path to the input surface file in VTK format.
#'
#' @return named list. The list has the following named entries: "vertices": nx3 double matrix, where n is the number of vertices. Each row contains the x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each row contains the vertex indices of the 3 vertices defining the face. WARNING: The indices are returned starting with index 1 (as used in GNU R). Keep in mind that you need to adjust the index (by substracting 1) to compare with data from other software.
#'
#' @note This is by far not a complete VTK format reader. Files that store
#'   streamlines instead of a mesh (i.e., that contain a LINES section) are read
#'   with \code{\link{read.fs.tracts.vtk}}.
#'
#' @family mesh functions
#'
#' @examples
#' surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
#' mesh <- read.fs.surface.vtk(surface_file)
#' cat(sprintf("Read a mesh with %d vertices and %d faces.\n", nrow(mesh$vertices), nrow(mesh$faces)))
#'
#' @export
read.fs.surface.vtk <- function(filepath) {
  polydata <- read.vtk.legacy.polydata(filepath)

  if (!is.null(polydata$polys)) {
    vtk.check.triangles(polydata$polys, filepath)
    num_faces <- length(polydata$polys)
    if (num_faces == 0L) {
      faces <- matrix(integer(0L), nrow = 0L, ncol = 3L)
    } else {
      faces <- matrix(unlist(polydata$polys, use.names = FALSE), ncol = 3L, byrow = TRUE) + 1L # the +1 is because the surface should use R indices (one-based)
    }
    ret_list <- list(
      vertices = polydata$points,
      faces = faces
    )
    class(ret_list) <- c("fs.surface", class(ret_list))
    return(ret_list)
  }

  if (!is.null(polydata$lines)) {
    stop(sprintf(
      "File '%s' contains streamlines (a LINES section) instead of a mesh. Use 'read.fs.tracts.vtk' to read it.\n",
      filepath
    ))
  }
  if (!is.null(polydata$verts)) {
    stop(sprintf(
      "File '%s' contains only a point cloud (a VERTICES section) and no polygons, so it is not a mesh.\n",
      filepath
    ))
  }
  stop(sprintf("File '%s' does not contain any polygons, so it is not a mesh.\n", filepath))
}


#' @title Read VTK legacy format streamlines as tracts.
#'
#' @description Reads streamline (tractography) data from files in the VTK legacy
#'   format, i.e. from POLYDATA datasets that contain a LINES section. Both the
#'   ASCII and the binary encoding are supported, as are the cell array layouts
#'   written by VTK 4.2 and older and by VTK 5.1 and newer. This is the format
#'   that Paraview, TrackVis and DSI Studio export streamlines in. Attribute data
#'   such as scalar values per point is ignored.
#'
#' @param filepath character string, path to the input file in VTK legacy format.
#'
#' @return an \code{\link{fs.tracts}} instance with the streamlines. Use
#'   \code{tracts[[i]]} to get the n x 3 coordinate matrix of a single
#'   streamline, and \code{\link{fs.tracts.lengths}} for the number of points of
#'   each streamline.
#'
#' @note Points that are not part of any streamline are dropped, because the
#'   \code{fs.tracts} data structure stores the coordinates of the streamlines
#'   without gaps. A warning is issued if that happens.
#'
#' @examples
#' # The example file was written by VTK and contains 3 streamlines.
#' # It also has a point that is not part of any streamline, which is dropped with a warning.
#' tracts_file <- system.file("extdata", "tracts_v51_binary.vtk",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' tracts <- suppressWarnings(read.fs.tracts.vtk(tracts_file))
#' cat(sprintf(
#'   "Read %d streamlines with %d points.\n",
#'   length(tracts), nrow(fs.tracts.coords(tracts))
#' ))
#'
#' @export
read.fs.tracts.vtk <- function(filepath) {
  polydata <- read.vtk.legacy.polydata(filepath)

  if (is.null(polydata$lines)) {
    if (!is.null(polydata$polys)) {
      stop(sprintf(
        "File '%s' contains polygons instead of streamlines (no LINES section). Use 'read.fs.surface.vtk' to read it as a mesh.\n",
        filepath
      ))
    }
    stop(sprintf("File '%s' does not contain a LINES section, so it contains no streamlines.\n", filepath))
  }

  lengths <- vapply(polydata$lines, length, integer(1L))
  indices <- unlist(polydata$lines, use.names = FALSE)
  if (length(indices) > 0L && (any(is.na(indices)) || any(indices < 0L) || any(indices >= nrow(polydata$points)))) {
    stop(sprintf("File '%s' contains streamlines with vertex indices that are out of range.\n", filepath))
  }

  coords <- polydata$points
  used_vertices <- sort(unique(indices))
  if (length(used_vertices) != nrow(coords)) {
    warning(sprintf(
      "File '%s' contains %d point(s) that are not part of any streamline. They were dropped, because the returned tracts object stores the streamlines without gaps.\n",
      filepath, nrow(coords) - length(used_vertices)
    ))
    indices <- match(indices, used_vertices) - 1L
    coords <- coords[used_vertices + 1L, , drop = FALSE]
  }

  return(fs.tracts(coords, lengths, kind = "tck"))
}
