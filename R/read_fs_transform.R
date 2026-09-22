#' @title  Load transformation matrix from a file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @param format character string, the file format. Currently 'auto' (guess based on file extension), 'xfm' (for xform format) or 'dat' (for tkregister style, e.g. register.dat) are supported.
#'
#' @return an `fs.transform` instance, see \code{\link{fs.transform}}. Its fields include the 'matrix', and the
#'   coordinate spaces the matrix maps between (`space_in`, `space_out` and `voxel_base`). Which of them are
#'   known depends on the format: an xfm file states neither (both sides are RAS), a register.dat file states
#'   both by definition, and an LTA file states them in its header.
#'
#' @note Currently this function has been tested with linear transformation files only, all others are unsupported.
#'
#' @family header coordinate space
#'
#' @examples
#' tf_file <- system.file("extdata", "talairach.xfm",
#'   package = "freesurferformats",
#'   mustWork = TRUE
#' )
#' transform <- read.fs.transform(tf_file)
#' transform$matrix
#'
#' @export
read.fs.transform <- function(filepath, format = "auto") {
  if (!format %in% c("auto", "xfm", "dat", "lta")) {
    stop("Format must be one of 'auto', 'xfm', 'dat', or 'lta'.")
  }

  if (format == "xfm" || (format == "auto" & endsWith(filepath, ".xfm"))) {
    return(read.fs.transform.xfm(filepath))
  }
  if (format == "dat" || (format == "auto" & endsWith(filepath, ".dat"))) {
    return(read.fs.transform.dat(filepath))
  }
  if (format == "lta" || (format == "auto" & endsWith(filepath, ".lta"))) {
    return(read.fs.transform.lta(filepath))
  }
  stop("Could not auto-detect transform file format from file extension, please specify.")
}


#' @title  Load transformation matrix from an XFM file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return an `fs.transform` instance. An xfm file stores a linear transformation between two RAS (world)
#'   coordinate spaces, typically the scanner space of a subject and the RAS space of an MNI or Talairach
#'   template, so `space_in` and `space_out` are both 'ras' and `voxel_base` is `NA`. The volumes that the
#'   transformation relates are not recorded in the file, so `src` and `dst` are `NULL`.
#'
#' @note Currently this function has been tested with linear transformation files only, all others are unsupported.
#'
#' @family header coordinate space
#'
#' @examples
#' tf_file <- system.file("extdata", "talairach.xfm",
#'   package = "freesurferformats",
#'   mustWork = TRUE
#' )
#' transform <- read.fs.transform.xfm(tf_file)
#' transform$matrix
#'
#' @export
read.fs.transform.xfm <- function(filepath) {
  transform <- list("type" = NULL, "matrix" = NULL)

  all_lines <- readLines(filepath)
  current_line_idx <- 1L
  while (current_line_idx <= length(all_lines)) {
    current_line <- all_lines[current_line_idx]
    if (startsWith(current_line, "Transform_Type")) {
      transform_type_definition_parts <- strsplit(current_line, "=")[[1]]
      transform_type_definition_parts_trimmed <- trimws(transform_type_definition_parts)
      transform$type <- trimws(transform_type_definition_parts_trimmed[2], which = "right")
      if (endsWith(transform$type, ";")) {
        transform$type <- substring(transform$type, 1L, (nchar(transform$type) - 1L))
      }
    }

    if (endsWith(trimws(current_line), "Transform =")) {
      if (length(all_lines) < current_line_idx + 3L) {
        stop(sprintf("Expected transformation matrix in lines %d-%d, but file only has %d lines.\n", current_line_idx + 1L, current_line_idx + 3L, length(all_lines)))
      }
      idx_matrix_start <- current_line_idx + 1L
      idx_matrix_end <- idx_matrix_start + 2L
      matrix_lines <- all_lines[idx_matrix_start:idx_matrix_end]
      transform$matrix <- parse.transform.matrix.lines(matrix_lines)
    }

    current_line_idx <- current_line_idx + 1L
  }

  if (is.null(transform$matrix)) {
    stop(sprintf("Found no 4x4 linear transformation matrix in xfm file '%s'. This function supports linear transformations only.\n", filepath))
  }

  # An xfm file stores a linear transformation between two RAS (world) coordinate spaces, typically the scanner
  # space of a subject and the RAS space of an MNI or Talairach template. The volumes themselves are not
  # recorded in the file, so neither the source nor the target can be described in more detail.
  return(fs.transform(
    matrix = transform$matrix, space_in = "ras", space_out = "ras",
    format = "xfm", source = filepath, type = transform$type
  ))
}


#' @title  Load transformation matrix from a tkregister dat file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return an `fs.transform` instance. A tkregister matrix maps the movable volume (the source) to the target
#'   volume, so `space_in` is 'voxel' and `space_out` is 'ras'. It produces RAS coordinates in the tkregister
#'   frame of the target volume, which is why `dst` states `frame = 'tkreg'`, see
#'   \code{\link{mghheader.vox2ras.tkreg}}. The intensity entry of the file is returned in the `intensity` field.
#'
#' @family header coordinate space
#'
#' @examples
#' tf_file <- system.file("extdata", "register.dat",
#'   package = "freesurferformats",
#'   mustWork = TRUE
#' )
#' transform <- read.fs.transform.dat(tf_file)
#' transform$matrix
#'
#' @export
read.fs.transform.dat <- function(filepath) {
  transform <- list("type" = NULL, "matrix" = NULL)

  all_lines <- readLines(filepath)
  if (length(all_lines) != 9L) {
    stop(sprintf("Expected 9 lines in tkregister dat file, found %d.\n", length(all_lines))) # nocov
  }

  # The intensity is a floating point value (FreeSurfer writes e.g. 0.15), reading it as an integer truncated it.
  transform$intensity <- as.numeric(trimws(all_lines[4]))
  transform$matrix <- parse.transform.matrix.lines(all_lines[5:8])

  # A tkregister matrix maps the movable volume (the source) to the target volume, which is not recorded in the
  # file. It consumes the voxel coordinates of the movable volume and produces RAS coordinates in the tkregister
  # frame of the target, which is RAS with an identity rotation and the origin at the center of the target
  # volume, see 'mghheader.vox2ras.tkreg'.
  return(fs.transform(
    matrix = transform$matrix, space_in = "voxel", space_out = "ras", voxel_base = 0L,
    dst = list("frame" = "tkreg"), format = "dat", source = filepath, type = transform$type,
    intensity = transform$intensity
  ))
}


#' @title  Load transformation matrix from a FreeSurfer linear transform array (LTA) file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return an `fs.transform` instance. The header of an LTA file states whether the matrix operates on voxel
#'   indices (type 0, LINEAR_VOX_TO_VOX) or on RAS world coordinates (type 1, LINEAR_RAS_TO_RAS), and that is
#'   used to set `space_in`, `space_out` and `voxel_base`. FreeSurfer voxel indices are zero-based. The file also
#'   records both volumes it relates, so `src` and `dst` contain the file name, the dimensions, the voxel size
#'   and the voxel-to-RAS matrix of each of them. The parsed header and volume info sections are kept in the
#'   `header` and `volumes` fields.
#'
#' @family header coordinate space
#'
#' @examples
#' tf_file <- system.file("extdata", "talairach.lta",
#'   package = "freesurferformats", mustWork = TRUE
#' )
#' transform <- read.fs.transform.lta(tf_file)
#' transform$matrix
#'
#' @note I found no spec for the LTA file format, only example files, so this function should be used with care. If you have a file that is not parsed correctly, please open an issue and attach it.
#'
#' @export
read.fs.transform.lta <- function(filepath) {
  transform <- list("type" = NULL, "matrix" = NULL)
  transform$header <- list()
  transform$volumes <- list()

  all_lines <- readLines(filepath)

  # Cleaning
  current_line_idx <- 0L
  for (tfline in all_lines) {
    current_line_idx <- current_line_idx + 1L
    # Cleanup: remove comments and extra whitespace
    if (startsWith(tfline, "#")) {
      next
    } # ignore comment lines
    if (length(strsplit(tfline, "#")[[1]]) > 1L) { # check whether a comment character occurs later in the line.
      tfline <- strsplit(tfline, "#")[[1]][1] # remove line parts after comment character (keep only part before 1st comment char).
    }
    all_lines[current_line_idx] <- trimws(tfline)
  }

  # Parsing
  current_line_idx <- 0L
  sections <- c("header", "matrix", "volume_info")
  current_section <- "header"
  current_volume <- NULL
  while (current_line_idx < length(all_lines)) {
    current_line_idx <- current_line_idx + 1L
    tfline <- all_lines[current_line_idx]

    # cat(sprintf("At line %d: '%s'\n", current_line_idx, tfline));

    if (startsWith(tfline, "#")) {
      next
    } # ignore comment lines
    if (nchar(tfline) < 1L) {
      next
    } # ignore empty lines

    # parse data
    if (current_section == "header") {
      if (length(strsplit(tfline, "=")[[1]]) > 1L) { # It's a line of the form key = value
        lkey <- trimws(strsplit(tfline, "=")[[1]][1])
        lvalue <- trimws(strsplit(tfline, "=")[[1]][2])
        transform$header[[lkey]] <- lvalue
      } else {
        transform$header$matrixdim <- scann(tfline, 3L, line_number = current_line_idx)
        current_section <- sections[2]
        num_matrix_rows <- transform$header$matrixdim[2]
        matrix_start <- current_line_idx + 1L
        matrix_end <- current_line_idx + num_matrix_rows
        # cat(sprintf("Parsing %d matrix rows from file lines %d to %d.\n", num_matrix_rows, matrix_start, matrix_end));
        transform$matrix <- parse.transform.matrix.lines(all_lines[matrix_start:matrix_end])
        current_line_idx <- current_line_idx + num_matrix_rows
        current_section <- sections[3]
      }
    } else if (current_section == "volume_info") {
      if (endsWith(tfline, "volume info")) {
        current_volume <- scann(tfline, 3L, what = character(), line_number = current_line_idx)[1]
        transform$volumes[[current_volume]] <- list()
      } else {
        if (is.null(current_volume)) {
          warning(sprintf("Skipping line '%s' number %d in LTA file volume_info section: no volume defined yet.\n", tfline, current_line_idx)) # nocov
        } else {
          lkey <- trimws(strsplit(tfline, "=")[[1]][1])
          lvalue <- trimws(strsplit(tfline, "=")[[1]][2])
          if (lkey %in% c("voxelsize", "xras", "yras", "zras", "cras")) {
            transform$volumes[[current_volume]][[lkey]] <- scann(lvalue, 3L, what = numeric(), line_number = current_line_idx)
          } else if (lkey %in% c("volume")) {
            transform$volumes[[current_volume]][[lkey]] <- scann(lvalue, 3L, what = integer(), line_number = current_line_idx)
          } else {
            transform$volumes[[current_volume]][[lkey]] <- lvalue
          }
        }
      }
    } else {
      stop(sprintf("Invalid LTA file section '%s' reached while parsing.\n", current_section))
    }
  }

  if (is.null(transform$matrix)) {
    stop(sprintf("Found no 4x4 transformation matrix in LTA file '%s'.\n", filepath))
  }

  # The header states whether the matrix maps voxel indices or RAS coordinates. Type 0 is LINEAR_VOX_TO_VOX and
  # type 1 is LINEAR_RAS_TO_RAS. FreeSurfer's voxel coordinates are zero-based, i.e., the first voxel is index
  # 0 and the center of the volume is at voxel index dim/2, see 'mghheader.vox2ras'.
  lta_type <- suppressWarnings(as.integer(transform$header$type))
  if (length(lta_type) != 1L) {
    lta_type <- NA_integer_ # the file does not state a type, or states something that is not a number
  }
  space_in <- NA_character_
  space_out <- NA_character_
  voxel_base <- NA_integer_
  if (is.na(lta_type)) {
    warning(sprintf("LTA file '%s' does not state a transformation type, cannot determine the coordinate spaces of its matrix.\n", filepath))
  } else if (lta_type == 0L) {
    space_in <- "voxel"
    space_out <- "voxel"
    voxel_base <- 0L
  } else if (lta_type == 1L) {
    space_in <- "ras"
    space_out <- "ras"
  } else {
    warning(sprintf("LTA file '%s' uses unsupported transformation type %d, cannot determine the coordinate spaces of its matrix.\n", filepath, lta_type))
  }

  volume_descriptor_for <- function(volume_info) {
    if (is.null(volume_info)) {
      return(NULL)
    }
    return(volume.descriptor(
      path = volume_info$filename, dim = volume_info$volume, voxelsize = volume_info$voxelsize,
      xras = volume_info$xras, yras = volume_info$yras, zras = volume_info$zras, cras = volume_info$cras,
      valid = volume_info$valid
    ))
  }

  return(fs.transform(
    matrix = transform$matrix, space_in = space_in, space_out = space_out, voxel_base = voxel_base,
    src = volume_descriptor_for(transform$volumes$src), dst = volume_descriptor_for(transform$volumes$dst),
    format = "lta", source = filepath, type = transform$header$type,
    header = transform$header, volumes = transform$volumes
  ))
}


#' @title Scan exactly n values from source string.
#'
#' @param cstring the input character string
#'
#' @param num integer, the number of expected resulting items.
#'
#' @param line_number optional integer, the line number (if the string represents a line from a text file). Will be printed in error message, if any.
#'
#' @return vector of type integer or double
#' @keywords internal
scann <- function(cstring, num = 1L, what = integer(), line_number = NULL) {
  res <- scan(text = cstring, what = what, quiet = TRUE)
  if (length(res) == num) {
    return(res)
  }
  line_info <- ifelse(is.null(line_number), "", sprintf(" at line %d", line_number))
  stop(sprintf("Expected %d entries but found %d in '%s'%s.\n", num, length(res), cstring, line_info))
}


#' @title Parse matrix from text file lines.
#'
#' @param file_lines vector of 3 character strings, the matrix lines. The separator is assumed to be a single space.
#'
#' @param ignore_line_suffix character string, a line suffix that will be stripped from the end of each line if it exists.
#'
#' @return numerical 4x4 matrix, the parsed matrix
#'
#' @keywords internal
parse.transform.matrix.lines <- function(file_lines, ignore_line_suffix = ";") {
  tm <- matrix(rep(0., 16L), ncol = 4L)
  tm[4, ] <- c(0, 0, 0, 1) # Will be overwritten below if 4 file_lines are given.

  if (!length(file_lines) %in% c(3L, 4L)) {
    stop(sprintf("Parameter 'lines' must be of length 3 or 4, is %d.\n", length(file_lines)))
  }

  line_idx <- 1L
  for (file_line in file_lines) {
    file_line <- trimws(file_line)

    if (endsWith(file_line, ignore_line_suffix)) {
      file_line <- substring(file_line, 1L, (nchar(file_line) - nchar(ignore_line_suffix)))
    }
    matrix_row <- as.double(strsplit(trimws(file_line), " ")[[1]])
    tm[line_idx, ] <- matrix_row
    line_idx <- line_idx + 1L
  }
  return(tm)
}
