#' @title  Load transformation matrix from a file.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @param format character string, the file format. 'auto' guesses it from the file extension and the file content, and 'xfm' (for xform format), 'dat' (for tkregister style, e.g. register.dat), 'lta' (for FreeSurfer LTA), 'fslmat' (for an FSL/FLIRT matrix) and 'itk' (for an ITK text transform, e.g. a `.tfm` file) can be given explicitly.
#'
#' @return an `fs.transform` instance, see \code{\link{fs.transform}}. Its fields include the 'matrix', and the
#'   coordinate spaces the matrix maps between (`space_in`, `space_out` and `voxel_base`). Which of them are
#'   known depends on the format: an xfm file states neither (both sides are RAS), a register.dat file states
#'   both by definition, an FSL matrix maps voxel coordinates (zero-based) to voxel coordinates, and an LTA file
#'   states the spaces in its header.
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
  supported_formats <- c("auto", "xfm", "dat", "lta", "fslmat", "itk")
  if (!format %in% supported_formats) {
    stop(sprintf("Format must be one of %s.\n", paste(supported_formats, collapse = ", ")))
  }

  if (format == "auto") {
    format <- guess.transform.format(filepath)
  }

  if (format == "xfm") {
    return(read.fs.transform.xfm(filepath))
  }
  if (format == "dat") {
    return(read.fs.transform.dat(filepath))
  }
  if (format == "lta") {
    return(read.fs.transform.lta(filepath))
  }
  if (format == "fslmat") {
    return(read.fs.transform.fslmat(filepath))
  }
  if (format == "itk") {
    return(read.fs.transform.itk(filepath))
  }
  stop(sprintf("Could not read transformation file '%s'.\n", filepath)) # nocov
}


#' @title Read a transformation matrix from an ITK text transform file.
#'
#' @description Read the plain text file format that ITK and the tools built on it (3D Slicer, ANTs via
#'   `ConvertTransformFile`, SimpleITK and the workflows of fMRIPrep/QSIPrep that are based on them) use to store
#'   linear transformations, usually with the extension `.tfm` or `.txt`.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return an `fs.transform` instance. An ITK transform operates on the world coordinates of the images, which in
#'   ITK are left-posterior-superior, so `space_in` and `space_out` are 'lps' and `voxel_base` is `NA`. This is
#'   not the RAS space that the other formats of this package use, and it is not converted silently: use
#'   \code{\link{transform2ras}} to get a transformation in RAS coordinates. The volumes are not recorded in
#'   the file, so `src` and `dst` are `NULL`. The ITK class name (e.g. 'AffineTransform_float_3_3') is stored in
#'   the `type` field, and the values of the `FixedParameters` entry in the `fixed_parameters` field.
#'
#' @note The format can store many kinds of transforms besides affine ones; this function reads the affine
#'   transformations only, i.e. the classes 'AffineTransform_float_3_3', 'AffineTransform_double_3_3',
#'   'MatrixOffsetTransformBase_float_3_3' and 'MatrixOffsetTransformBase_double_3_3'. These are the classes
#'   that occur in the output of the pipelines mentioned above, and the only ones for which the interpretation
#'   of the parameters could be verified against other implementations. Files that contain several transformations
#'   (an ITK 'CompositeTransform') are not supported either, and are reported as such: composing them requires
#'   the ordering rules of ITK, which would be a guess without a reference to check against.
#'
#'   The `FixedParameters` entry is the center of rotation, so the matrix that is returned is
#'   `y = A(x - c) + t + c`, i.e. it has the center folded in. That is the same thing that the ITK writer of
#'   this package stores, and the transformation is not changed by it.
#'
#'   FreeSurfer reads ITK files as well, but two limitations of its version 7.4.1 are worth knowing when the file
#'   has to be passed to it: it rejects the 'float' variant of the classes ('readITK: Transform type unknown!'),
#'   and its `lta_convert --initk` ignores the `FixedParameters`, so it interprets a file with a non-zero center
#'   of rotation differently from ITK itself (which computes `offset = translation + center - matrix * center`,
#'   see `ComputeOffset()` in ITK's `itkMatrixOffsetTransformBase.hxx`) and from this package. Both were verified
#'   by converting files that encode the same transformation, and both are avoided by the files that
#'   \code{\link{write.fs.transform.itk}} writes.
#'
#' @examples
#' xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
#' tf <- transform2ras(transform2lps(read.fs.transform(xfm_file)))
#' summary(tf)$space_in
#'
#' @family header coordinate space
#'
#' @export
read.fs.transform.itk <- function(filepath) {
  all_lines <- tryCatch(readLines(filepath, warn = FALSE), error = function(e) character(0))
  first_line <- if (length(all_lines) > 0L) trimws(all_lines[1L]) else ""

  if (!startsWith(first_line, "#Insight Transform")) {
    sniffed_text <- transform.file.sniff.text(filepath)
    if (grepl("Transform_float_3_3|Transform_double_3_3", sniffed_text)) {
      stop(sprintf("Transformation file '%s' is a binary ITK or ANTs transform (e.g. the '.mat' file that ANTs writes), which is not supported by this package. Convert it to the text form with the ITK tools first, e.g. 'ConvertTransformFile 3 in.mat out.txt'.\n", filepath))
    }
    stop(sprintf("Transformation file '%s' is not an ITK text transform: expected it to start with the header line '#Insight Transform File V1.0'.\n", filepath))
  }

  block_lines <- grep("^#Transform[[:space:]]+[0-9]+$", trimws(all_lines))
  if (length(block_lines) == 0L) {
    stop(sprintf("Transformation file '%s' contains no '#Transform' block.\n", filepath))
  }
  if (length(block_lines) > 1L) {
    stop(sprintf("Transformation file '%s' contains %d transformations. Composing them is not supported, only files with a single transformation can be read.\n", filepath, length(block_lines)))
  }

  # The key/value lines of the block, e.g. 'Transform: AffineTransform_float_3_3'.
  key_lines <- trimws(all_lines)
  key_lines <- key_lines[nzchar(key_lines) & !startsWith(key_lines, "#")]

  transform_class <- itk.key.value(key_lines, "Transform")
  if (is.null(transform_class)) {
    stop(sprintf("Transformation file '%s' does not state a 'Transform' entry, the type of the transformation is unknown.\n", filepath))
  }
  supported_classes <- c(
    "AffineTransform_float_3_3", "AffineTransform_double_3_3",
    "MatrixOffsetTransformBase_float_3_3", "MatrixOffsetTransformBase_double_3_3"
  )
  if (!(transform_class %in% supported_classes)) {
    stop(sprintf("Unsupported ITK transform class '%s' in file '%s'. This package reads the affine transformations %s. Transformations of other classes (e.g. 'Euler3DTransform_*', 'VersorRigid3DTransform_*', 'BSplineTransform_*', 'DisplacementFieldTransform_*') are not supported.\n", transform_class, filepath, paste(supported_classes, collapse = ", ")))
  }

  parameters <- itk.numeric.value(key_lines, "Parameters", filepath)
  if (length(parameters) != 12L) {
    stop(sprintf("Expected 12 parameters for ITK affine transform '%s' in file '%s', found %d.\n", transform_class, filepath, length(parameters)))
  }
  fixed_parameters <- itk.numeric.value(key_lines, "FixedParameters", filepath)
  if (length(fixed_parameters) < 3L) {
    stop(sprintf("Expected at least 3 values in the 'FixedParameters' entry (the center of rotation) in file '%s', found %d.\n", filepath, length(fixed_parameters)))
  }

  # ITK serialises the linear part row by row, and the last three parameters are the translation. The transform
  # is 'y = A(x - c) + t + c', where c is the center of rotation from the fixed parameters, so the center has to
  # be folded into the translation to get a plain affine matrix.
  linear <- matrix(parameters[1:9], ncol = 3L, byrow = TRUE)
  translation <- parameters[10:12]
  centre <- fixed_parameters[1:3]
  transformed <- rbind(cbind(linear, translation + centre - (linear %*% centre)), c(0, 0, 0, 1))

  return(fs.transform(
    matrix = transformed, space_in = "lps", space_out = "lps",
    format = "itk", source = filepath, type = transform_class,
    fixed_parameters = fixed_parameters
  ))
}


#' @title Read the value of a key of an ITK transform file.
#'
#' @description ITK text transforms store their content as 'key: value' lines, e.g. 'Transform:
#'   AffineTransform_float_3_3'. This helper returns the value of such an entry.
#'
#' @param lines character vector, the key/value lines of the file.
#'
#' @param key character string, the name of the entry, e.g. 'Transform'.
#'
#' @return `NULL` if the entry does not exist, its value as a character string otherwise.
#'
#' @keywords internal
itk.key.value <- function(lines, key) {
  matching <- lines[startsWith(lines, paste0(key, ":"))]
  if (length(matching) == 0L) {
    return(NULL)
  }
  return(trimws(substring(matching[1L], nchar(key) + 2L)))
}


#' @title Read a numerical entry of an ITK transform file.
#'
#' @param lines character vector, the key/value lines of the file.
#'
#' @param key character string, the name of the entry, e.g. 'Parameters'.
#'
#' @param filepath character string, the path of the file, used in error messages only.
#'
#' @return numerical vector, the values of the entry. It is an error if the entry is missing or holds no numbers.
#'
#' @keywords internal
itk.numeric.value <- function(lines, key, filepath) {
  value <- itk.key.value(lines, key)
  if (is.null(value)) {
    stop(sprintf("Transformation file '%s' has no '%s' entry.\n", filepath, key))
  }
  values <- suppressWarnings(as.numeric(strsplit(value, "[[:space:]]+")[[1L]]))
  if (length(values) == 0L || any(is.na(values))) {
    stop(sprintf("Could not parse the '%s' entry of transformation file '%s', its value is '%s'.\n", key, filepath, value))
  }
  return(values)
}


#' @title Read a transformation matrix from an FSL matrix file.
#'
#' @description Read the plain text 4x4 matrix that FSL's `flirt` writes with the `-omat` option, and that FSL,
#'   MRtrix3 and FreeSurfer read as the registration between two images.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return an `fs.transform` instance. An FSL matrix maps the voxel coordinates of the image given to `flirt
#'   -in` to those of the image given to `flirt -ref`. Both are voxel indices, so `space_in` and `space_out` are
#'   'voxel' and `voxel_base` is 0 (FSL voxel indices are zero-based). The two images are not recorded in the
#'   file, so `src` and `dst` are `NULL` and the volumes have to be passed to \code{\link{transform2world}}
#'   to interpret the matrix in world coordinates.
#'
#' @examples
#' # Write the example LTA as an FSL matrix and read it back: no FSL installation is needed for that.
#' lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
#' mat_file <- tempfile(fileext = ".mat")
#' write.fs.transform(read.fs.transform(lta_file), mat_file, format = "fslmat")
#' read.fs.transform(mat_file)$matrix
#' unlink(mat_file)
#'
#' @family header coordinate space
#'
#' @export
read.fs.transform.fslmat <- function(filepath) {
  all_lines <- readLines(filepath)
  all_lines <- trimws(all_lines)
  all_lines <- all_lines[nzchar(all_lines) & !startsWith(all_lines, "#")]

  if (length(all_lines) < 4L) {
    stop(sprintf("Expected 4 lines with 4 numerical values in FSL matrix file '%s', found %d lines with content.\n", filepath, length(all_lines)))
  }

  transformed <- matrix(NA_real_, nrow = 4L, ncol = 4L)
  for (line_idx in 1:4) {
    transformed[line_idx, ] <- scann(all_lines[line_idx], 4L, what = numeric(), line_number = line_idx)
  }

  return(fs.transform(
    matrix = transformed, space_in = "voxel", space_out = "voxel", voxel_base = 0L,
    format = "fslmat", source = filepath
  ))
}


#' @title Determine the format of a transformation file.
#'
#' @description Guess the format of a transformation file from its extension and its content. Content is needed
#' because the extension '.mat' is used by FSL for text matrices and by ANTs/ITK for binary transformations,
#' which have nothing in common. A file that is identified as an ITK/ANTs transformation is reported as such
#' instead of failing with a parse error, since that format is not supported yet.
#'
#' @param filepath character string, the full path to the transform file.
#'
#' @return character string, the file format, one of 'xfm', 'dat', 'lta' or 'fslmat'.
#'
#' @keywords internal
guess.transform.format <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("Transformation file '%s' does not exist.\n", filepath))
  }

  sniffed_text <- transform.file.sniff.text(filepath)
  itk_markers <- c(
    "Insight Transform", "AffineTransform_", "Euler3DTransform_", "MatrixOffsetTransformBase_",
    "VersorRigid3DTransform_", "DisplacementFieldTransform_", "CompositeTransform_"
  )
  if (any(sapply(itk_markers, function(marker) grepl(marker, sniffed_text, fixed = TRUE)))) {
    # Text and binary ITK transforms are both identified here. The reader reports the binary ones as unsupported.
    return("itk")
  }

  extension <- tolower(sub("^.*\\.", "", basename(filepath)))
  if (identical(extension, basename(filepath))) {
    extension <- "" # the file name contains no dot
  }
  if (extension %in% c("xfm", "dat", "lta")) {
    return(extension)
  }
  if (extension == "tfm") {
    return("itk")
  }
  if (extension == "mat") {
    return("fslmat")
  }

  content_lines <- readLines(filepath, n = 4L, warn = FALSE)
  if (length(content_lines) == 4L && all(sapply(content_lines, text.line.is.numeric, num = 4L))) {
    return("fslmat")
  }

  stop(sprintf("Could not determine the format of transformation file '%s', please use the 'format' parameter.\n", filepath))
}


#' @title Extract the text of a file for format sniffing.
#'
#' @description Read the beginning of a file and return the printable characters it contains, so that the file
#'   can be identified by markers in its content without failing on binary data.
#'
#' @param filepath character string, the path to the file.
#'
#' @param num_bytes integer, the number of bytes to inspect.
#'
#' @return character string, the printable characters of the beginning of the file.
#'
#' @keywords internal
#'
#' @exportS3Method NULL
transform.file.sniff.text <- function(filepath, num_bytes = 256L) {
  raw_bytes <- readBin(filepath, "raw", n = num_bytes)
  if (length(raw_bytes) == 0L) {
    return("")
  }
  printable <- raw_bytes[raw_bytes >= as.raw(32L) & raw_bytes < as.raw(127L)]
  return(rawToChar(printable))
}


#' @title Check whether a text line holds a fixed number of numerical values.
#'
#' @param line character string, the line to check.
#'
#' @param num integer, the number of numerical values expected in the line.
#'
#' @return logical, whether the line contains exactly `num` numerical values and nothing else.
#'
#' @keywords internal
#'
#' @exportS3Method NULL
text.line.is.numeric <- function(line, num) {
  fields <- strsplit(trimws(line), "[[:space:]]+")[[1L]]
  if (length(fields) != num) {
    return(FALSE)
  }
  values <- suppressWarnings(as.numeric(fields))
  return(!any(is.na(values)))
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
#'   \code{\link{mghheader.vox2ras.tkreg}}. The other entries of the file are kept as the `subject`,
#'   `in_plane_resolution`, `between_plane_resolution` and `intensity` fields.
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
    subject = trimws(all_lines[1]),
    in_plane_resolution = as.numeric(trimws(all_lines[2])),
    between_plane_resolution = as.numeric(trimws(all_lines[3])),
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
