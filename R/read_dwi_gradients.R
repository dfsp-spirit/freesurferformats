# Readers for diffusion MRI gradient tables, aka b-vectors/b-values, aka the
# DW encoding or DW gradient scheme. Both the FSL bvecs/bvals format and the
# MRtrix3 gradient table format are supported.
#
# The semantics implemented here follow these references:
#  - FSL bvecs/bvals layout and the meaning of the vectors (they are given with
#    respect to the *image* axes, not the scanner axes):
#    https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FDT/FAQ#What_conventions_do_the_bvecs_use.3F
#  - MRtrix3 DW gradient table handling, including the definition of the
#    MRtrix format (one row per volume, columns [ x y z b ], directions in
#    scanner space) and the FSL format:
#    https://mrtrix.readthedocs.io/en/latest/concepts/dw_scheme.html
#  - MRtrix3 'core/dwi/gradient.cpp', functions load_bvecs_bvals() and
#    save_bvecs_bvals(), for the transpose rules, the NA/NaN handling and the
#    b=0 threshold. This is the implementation we cross-checked against.


#' @title Get the b-value threshold below which a volume counts as a b=0 volume.
#'
#' @description MRtrix3 treats any volume whose b-value is at most this
#'   threshold as a `b=0` volume. See the `BZeroThreshold` configuration entry
#'   of MRtrix3, which defaults to 10 s/mm^2.
#'
#' @return numeric scalar, the threshold in s/mm^2.
#'
#' @keywords internal
.dti.bzero.threshold <- function() {
  return(10.0)
}


#' @title Read a whitespace-separated numeric table from a text file.
#'
#' @description Low-level parser shared by all gradient table readers. Values
#'   may be separated by spaces, tabs or commas, and both blank lines and lines
#'   starting with `#` are ignored. All remaining lines must contain the same
#'   number of values.
#'
#' @param filepath character string, path to the file. Gzip-compressed files
#'   are detected by their magic bytes, not by the file name.
#'
#' @param allow_count_header logical, whether a first line that consists of a
#'   single integer may be interpreted as a count header and dropped. Only the
#'   MRtrix gradient table reader enables this, because the historical
#'   `.grad`/`.b` files may start with the number of volumes.
#'
#' @return a numeric matrix, one row per input line.
#'
#' @keywords internal
.read.numeric.table <- function(filepath, allow_count_header = FALSE) {
  if (!is.character(filepath) || length(filepath) != 1L) {
    stop("Parameter 'filepath' must be a character string.")
  }
  if (!file.exists(filepath)) {
    stop(sprintf("File '%s' does not exist.", filepath))
  }
  if (file.info(filepath)$isdir) {
    stop(sprintf("File '%s' is a directory, not a file.", filepath))
  }

  con <- if (is.gzip.file(filepath)) gzfile(filepath, "rt") else file(filepath, "rt")
  on.exit(
    {
      close(con)
    },
    add = TRUE
  )

  lines <- readLines(con, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!startsWith(lines, "#")]

  if (length(lines) < 1L) {
    stop(sprintf("File '%s' does not contain any data.", filepath))
  }

  rows <- lapply(seq_along(lines), function(line_idx) {
    fields <- strsplit(lines[line_idx], "[[:space:]]+|,", perl = TRUE)[[1]]
    fields <- fields[nzchar(fields)]
    values <- suppressWarnings(as.numeric(fields))
    if (length(values) < 1L || anyNA(values)) {
      stop(sprintf("Line %d of file '%s' does not contain numeric values only: '%s'", line_idx, filepath, lines[line_idx]))
    }
    return(values)
  })

  if (allow_count_header && length(rows) > 1L && length(rows[[1L]]) == 1L) {
    if (rows[[1L]] == (length(rows) - 1L)) {
      warning(sprintf("Interpreting the first line of file '%s' ('%d') as a gradient table count header and ignoring it. Check the file if this is not what you expect.", filepath, rows[[1L]]))
      rows <- rows[-1L]
    }
  }

  row_lengths <- vapply(rows, length, integer(1L))
  if (length(unique(row_lengths)) != 1L) {
    observed <- paste(sort(unique(row_lengths)), collapse = ", ")
    stop(sprintf("Malformed file '%s': expected all lines to contain the same number of values, but found lines with %s values.", filepath, observed))
  }

  num_rows <- length(rows)
  num_cols <- row_lengths[1L]
  validate_allocation_size(c(num_rows, num_cols), 8L, label = sprintf("the gradient table in file '%s'", filepath))

  return(matrix(unlist(rows, use.names = FALSE), nrow = num_rows, ncol = num_cols, byrow = TRUE))
}


#' @title Bring a gradient table into the canonical 'one row per volume' layout.
#'
#' @description Gradient tables occur in two orientations in the wild: the
#'   *FSL* layout, in which the first axis is the vector *component* (3 rows of
#'   N values for b-vectors, a single row of N values for b-values), and the
#'   *volumes* layout, in which each line is one *volume* (N rows of 3 values
#'   for b-vectors, as used by HCP, and N rows of 4 values for the MRtrix
#'   gradient table format). This helper maps both to one row per volume.
#'
#' @param m numeric matrix, the table as read from the file.
#'
#' @param n_components integer, the number of values per volume (3 for
#'   b-vectors, 4 for a gradient table, 1 for b-values).
#'
#' @param layout character string, one of 'auto', 'components' or 'volumes'.
#'   How to interpret the input. With 'auto' (the default), the orientation is
#'   derived from the matrix dimensions.
#'
#' @param what character string, a human-readable description of the data, used
#'   in the messages.
#'
#' @param square_is_ambiguous logical, how to handle a table that has exactly
#'   `n_components` rows *and* columns. The b-vector *file* reader sets this to
#'   `TRUE`, because for a 3x3 b-vector file the orientation cannot be decided
#'   (and the two reference implementations disagree about it). For a matrix
#'   passed in memory, and for the MRtrix gradient table, whose format defines
#'   one row per volume, the default `FALSE` means that the rows are read as
#'   volumes.
#'
#' @return a numeric matrix with one row per volume and `n_components` columns.
#'
#' @note A square 3x3 *file* is ambiguous, and the two reference implementations
#'   do not agree on it: MRtrix3 reads such a b-vectors file with the components
#'   in the rows, while DIPY reads it with the volumes in the rows. Like
#'   MRtrix3, \code{\link[freesurferformats]{read.dti.bvec}} treats the lines as
#'   components, and warns, because silently guessing here would scramble the
#'   gradient directions. Matrices passed in memory are read with one row per
#'   volume, which is the R convention, and no warning is needed.
#'
#' @keywords internal
.canonicalize.gradient.table <- function(m, n_components, layout, what, square_is_ambiguous = FALSE) {
  if (!(layout %in% c("auto", "components", "volumes"))) {
    stop("Parameter 'layout' must be one of 'auto', 'components' or 'volumes'.")
  }
  m <- as.matrix(m)
  if (!is.numeric(m)) {
    stop(sprintf("The %s must be numeric.", what))
  }
  storage.mode(m) <- "double"

  if (layout == "volumes") {
    if (ncol(m) != n_components) {
      stop(sprintf("Expected %d %s per line (one volume per line), but the table is %d x %d.", n_components, what, nrow(m), ncol(m)))
    }
    return(m)
  }

  if (layout == "components") {
    if (nrow(m) != n_components) {
      stop(sprintf("Expected %d lines (one line per component), but the table is %d x %d.", n_components, nrow(m), ncol(m)))
    }
    return(t(m))
  }

  # layout == 'auto'
  if (square_is_ambiguous && n_components > 1L && nrow(m) == n_components && ncol(m) == n_components) {
    warning(sprintf(
      "Ambiguous %s table: the table is %d x %d, so it cannot be told whether the lines are volumes or components. Interpreting the lines as components, which is what the FSL layout and MRtrix3 do. Pass layout = 'components' or layout = 'volumes' to make this explicit.",
      what, nrow(m), ncol(m)
    ))
    return(t(m))
  }
  if (ncol(m) == n_components) {
    return(m)
  }
  if (nrow(m) == n_components) {
    return(t(m))
  }
  stop(sprintf("The %s table must contain exactly %d rows or columns, but it is %d x %d.", what, n_components, nrow(m), ncol(m)))
}


#' @title Name the columns of an MRtrix gradient table.
#'
#' @param gt numeric matrix with 4 columns, one row per volume.
#'
#' @return the matrix with columns named 'x', 'y', 'z' and 'b'.
#'
#' @keywords internal
.name.gradient.table.columns <- function(gt) {
  colnames(gt) <- c("x", "y", "z", "b")
  return(gt)
}


#' @title Find the b-value file that belongs to a b-vector file.
#'
#' @description Looks for the `bvals` file next to a `bvecs` file, by replacing
#'   the `bvec`/`bvecs` part of the file name with `bval`/`bvals`, and by
#'   replacing the file extension if that does not match. This makes the
#'   readers work out of the box on standard datasets, e.g. the BIDS files
#'   `sub-01_dwi.bvec` and `sub-01_dwi.bval`.
#'
#' @param bvec_filepath character string, path to the b-vector file.
#'
#' @return character string, the path to the existing b-value file, or NULL if
#'   none of the candidates exists.
#'
#' @keywords internal
.find.bval.file <- function(bvec_filepath) {
  parent_dir <- dirname(bvec_filepath)
  base_name <- basename(bvec_filepath)

  candidate_names <- character(0)
  for (replacement in list(c("bvecs", "bvals"), c("bvec", "bval"), c("BVECS", "BVALS"), c("BVEC", "BVAL"), c("Bvecs", "Bvals"), c("Bvec", "Bval"))) {
    if (grepl(replacement[1L], base_name, fixed = TRUE)) {
      candidate_names <- c(candidate_names, sub(replacement[1L], replacement[2L], base_name, fixed = TRUE))
    }
  }
  candidate_names <- c(candidate_names, sub("\\.[^.]*$", ".bval", base_name), sub("\\.[^.]*$", ".bvals", base_name))

  candidate_paths <- file.path(parent_dir, unique(candidate_names))
  for (candidate_path in candidate_paths) {
    if (file.exists(candidate_path) && !file.info(candidate_path)$isdir) {
      return(candidate_path)
    }
  }
  return(NULL)
}


#' @title Read b-values from a FSL-style bvals file.
#'
#' @description Reads the b-values of a diffusion MRI dataset, i.e. the
#'   diffusion weighting of each volume in units of s/mm^2. This implements the
#'   *bvals* part of the FSL format: a plain text file with one value per
#'   volume, usually in a single row. The HCP variant, which stores one value
#'   per line, is read as well.
#'
#' @param filepath character string, path to the b-values file, typically
#'   ending in `.bval` or `.bvals`, but without an extension in the HCP layout.
#'
#' @param layout character string, one of 'auto', 'components' or 'volumes'.
#'   `components` means that the file stores the values in a single line (the
#'   FSL layout), `volumes` means one value per line (the HCP layout). With
#'   'auto' (the default), the layout is derived from the file content.
#'
#' @return numeric vector of length *n*, the b-value of each of the *n* volumes.
#'
#' @family dti functions
#'
#' @export
read.dti.bval <- function(filepath, layout = "auto") {
  m <- .read.numeric.table(filepath)
  m <- .canonicalize.gradient.table(m, 1L, layout, what = "b-value")
  bval <- as.double(as.vector(m))

  if (any(bval < 0)) {
    warning(sprintf("File '%s' contains negative b-values, which are not meaningful. The values are returned unmodified.", filepath))
  }
  if (any(is.infinite(bval))) {
    stop(sprintf("File '%s' contains infinite b-values.", filepath))
  }
  return(bval)
}


#' @title Read b-vectors from a FSL-style bvecs file.
#'
#' @description Reads the diffusion gradient directions of a diffusion MRI
#'   dataset, i.e. the *b-vectors* part of the FSL bvecs/bvals format. The file
#'   contains unit vectors (or zero vectors for the *b=0* volumes), stored as
#'   three rows of *n* values, one row per component, one column per volume.
#'
#' @param filepath character string, path to the b-vectors file, typically
#'   ending in `.bvec` or `.bvecs`, but without an extension in the HCP layout.
#'
#' @param layout character string, one of 'auto', 'components' or 'volumes'.
#'   `components` means that there are 3 lines, one per vector component (the
#'   FSL layout, and what the file name `bvecs` implies), `volumes` means one
#'   volume per line, i.e. *n* lines with 3 values each (the HCP layout). With
#'   'auto' (the default), the layout is derived from the matrix dimensions and
#'   a warning is raised for a square table, which is ambiguous.
#'
#' @return a numeric matrix with *n* rows and 3 columns, one row per volume, the
#'   columns being the x, y and z component of the gradient direction. Note that
#'   these vectors are given with respect to the *image* axes, not to the
#'   scanner axes, so they are only meaningful together with the image they
#'   belong to: converting them to scanner space requires the transform of that
#'   image. Use \code{\link[freesurferformats]{read.dti.gradients}} to read
#'   b-vectors and b-values together, with consistency checks.
#'
#' @note The b-vectors are returned exactly as they are stored, i.e. they are
#'   *not* renormalized, and missing values (see
#'   \code{\link[freesurferformats]{read.dti.gradients}}) are not replaced here.
#'
#' @family dti functions
#'
#' @export
read.dti.bvec <- function(filepath, layout = "auto") {
  m <- .read.numeric.table(filepath)
  bvec <- .canonicalize.gradient.table(m, 3L, layout, what = "b-vector", square_is_ambiguous = TRUE)
  colnames(bvec) <- c("x", "y", "z")

  if (any(is.infinite(bvec))) {
    stop(sprintf("File '%s' contains infinite b-vector components.", filepath))
  }
  return(bvec)
}


#' @title Read a gradient table in MRtrix3 format.
#'
#' @description Reads a diffusion gradient table in MRtrix format: a plain text
#'   file with one line per DWI volume, each line holding the four
#'   space-separated values `[ x y z b ]`, with the direction in *scanner*
#'   space and the b-value in s/mm^2. This is the format expected by the
#'   `-grad` option of the MRtrix3 commands (and produced by
#'   `-export_grad_mrtrix`), and it is also the format in which MRtrix3 stores
#'   the table in the `dw_scheme` entry of a `.mif` header. A first line
#'   consisting of a single integer is accepted as a volume count header.
#'
#' @param filepath character string, path to the file. There is no fixed file
#'   name convention, `grad.b` and `encoding.b` are common. Gzip-compressed
#'   files are supported.
#'
#' @param layout character string, one of 'auto', 'components' or 'volumes'.
#'   `volumes` means one volume per line (4 values per line, the MRtrix layout),
#'   `components` the transposed variant (4 lines). With 'auto' (the default),
#'   the layout is derived from the matrix dimensions.
#'
#' @return a numeric matrix with *n* rows and 4 columns named 'x', 'y', 'z' and
#'   'b', one row per volume. The directions are in scanner space.
#'
#' @family dti functions
#'
#' @export
read.dti.grad <- function(filepath, layout = "auto") {
  m <- .read.numeric.table(filepath, allow_count_header = TRUE)
  gt <- .canonicalize.gradient.table(m, 4L, layout, what = "gradient table")
  gt <- .name.gradient.table.columns(gt)

  if (any(is.infinite(gt))) {
    stop(sprintf("File '%s' contains infinite values.", filepath))
  }
  if (any(gt[, 4L] < 0, na.rm = TRUE)) {
    warning(sprintf("File '%s' contains negative b-values, which are not meaningful. The values are returned unmodified.", filepath))
  }
  return(gt)
}


#' @title Read and validate a diffusion MRI gradient table.
#'
#' @description The main entry point for reading diffusion gradients. It reads
#'   b-vectors and b-values from either a pair of FSL-style files or from a
#'   single MRtrix-style gradient table file, checks that they are consistent,
#'   and reports suspicious entries. Use this rather than calling
#'   \code{\link[freesurferformats]{read.dti.bvec}} and
#'   \code{\link[freesurferformats]{read.dti.bval}} separately if you intend to
#'   use the gradients for anything, because a mismatched or malformed gradient
#'   table is silently wrong otherwise.
#'
#' @details Both parameters accept either a file path or already read data, so
#'   the function can also be used to validate gradients that were obtained from
#'   somewhere else. If `bval` is `NULL` and `bvec` is a single file path, the
#'   file is interpreted as follows. A file whose name contains `bvec` is read
#'   as a b-vectors file (since the name is the user's clearest statement about
#'   the content, and a b-vectors file with 4 volumes would otherwise look
#'   exactly like a 3-volume gradient table). Otherwise, a file that has 4
#'   values per line, or 4 lines, is read as an MRtrix gradient table. Otherwise
#'   the file is read as b-vectors and a matching b-values file is looked up
#'   next to it, which covers the common dataset layouts such as BIDS
#'   (`sub-01_dwi.bvec` with `sub-01_dwi.bval`).
#'
#' @param bvec character string (path to a b-vectors file), or a numeric matrix
#'   with one row per volume (or one column per volume) and 3 columns.
#'
#' @param bval character string (path to a b-values file), or a numeric vector
#'   with one value per volume. Can be `NULL` if `bvec` identifies both files.
#'
#' @param n_volumes scalar numeric or integer, the number of volumes in the DWI
#'   image that the gradients belong to, used to check that the table matches
#'   the image. Typically `dim(volume$data)[4]`. Set to `NULL` (the default) to
#'   skip this check.
#'
#' @param layout character string, passed on to the readers, one of 'auto',
#'   'components' or 'volumes'.
#'
#' @return a named list with the entries `bvec` (numeric matrix with *n* rows
#'   and 3 columns, one row per volume, components in *image* space) and `bval`
#'   (numeric vector of length *n*).
#'
#' @note The following problematic cases are reported, but the data is always
#'   returned as read, except for the missing values, which are replaced by
#'   zeros: (1) `NA`/`NaN` entries, which are interpreted as `b=0` volumes, as
#'   MRtrix3 does, provided that they do not leave a volume with a b-value but
#'   no direction or vice versa, which is an error; (2) gradient vectors whose
#'   norm deviates from 1, which is reported because MRtrix3 rescales the
#'   b-value by the squared norm in this situation; (3) volumes with a b-value
#'   above the b=0 threshold but a zero direction; (4) volumes with a small
#'   positive b-value but a non-zero direction.
#'
#' @family dti functions
#'
#' @export
read.dti.gradients <- function(bvec, bval = NULL, n_volumes = NULL, layout = "auto") {
  if (is.null(bval) && is.character(bvec) && length(bvec) == 1L) {
    file_is_bvec <- grepl("bvec", basename(bvec), ignore.case = TRUE)
    m <- .read.numeric.table(bvec, allow_count_header = !file_is_bvec)
    if (!file_is_bvec && (ncol(m) == 4L || nrow(m) == 4L)) {
      gt <- .name.gradient.table.columns(.canonicalize.gradient.table(m, 4L, layout, what = "gradient table"))
      if (any(gt[, 4L] < 0, na.rm = TRUE)) {
        warning(sprintf("File '%s' contains negative b-values, which are not meaningful. The values are returned unmodified.", bvec))
      }
      return(validate.dti.gradients(gt[, 1:3, drop = FALSE], gt[, 4L], n_volumes = n_volumes))
    }

    bval_file <- .find.bval.file(bvec)
    if (is.null(bval_file)) {
      stop(sprintf(
        "File '%s' is not an MRtrix gradient table (which would need 4 values per volume) and no matching b-values file was found next to it. Either pass the b-values file explicitly as the 'bval' argument, or name the files like the FSL tools do (e.g. 'dwi.bvec' and 'dwi.bval').",
        bvec
      ))
    }
    bval <- bval_file
  }

  bvec_data <- if (is.character(bvec)) read.dti.bvec(bvec, layout = layout) else bvec
  bval_data <- if (is.character(bval)) read.dti.bval(bval, layout = layout) else bval

  return(validate.dti.gradients(bvec_data, bval_data, n_volumes = n_volumes))
}


#' @title Validate and normalize a diffusion MRI gradient table.
#'
#' @description Checks that a set of b-vectors and b-values is consistent,
#'   replaces missing values by `b=0` volumes, and reports suspicious entries.
#'   This is the validation used by
#'   \code{\link[freesurferformats]{read.dti.gradients}}, but it can also be
#'   called directly on gradients from any other source.
#'
#' @inheritParams read.dti.gradients
#'
#' @return a named list with the entries `bvec` (numeric matrix with *n* rows
#'   and 3 columns, one row per volume) and `bval` (numeric vector of length
#'   *n*).
#'
#' @note The rules for missing values match those of MRtrix3: a missing b-value
#'   in a volume with a valid direction, or a missing direction in a volume with
#'   a non-zero b-value, is an error, because such a volume cannot be
#'   interpreted. Everything else is treated as a `b=0` volume. The norm of the
#'   gradient vectors is never changed, and a b-value is never rescaled, because
#'   that would silently alter the data.
#'
#' @keywords internal
validate.dti.gradients <- function(bvec, bval, n_volumes = NULL) {
  if (is.data.frame(bvec)) {
    bvec <- as.matrix(bvec)
  }
  if (!is.matrix(bvec) || !is.numeric(bvec)) {
    stop("Parameter 'bvec' must be a numeric matrix, with one row per volume (or one column per volume) and 3 columns.")
  }
  bvec <- .canonicalize.gradient.table(bvec, 3L, layout = "auto", what = "b-vector")

  if (is.data.frame(bval)) {
    bval <- as.matrix(bval)
  }
  if (is.matrix(bval)) {
    if (nrow(bval) > 1L && ncol(bval) > 1L) {
      stop(sprintf("Parameter 'bval' must be a numeric vector, or a matrix with a single row or column, but it is %d x %d.", nrow(bval), ncol(bval)))
    }
    bval <- as.vector(bval)
  }
  if (!is.numeric(bval)) {
    stop("Parameter 'bval' must be numeric.")
  }
  bval <- as.double(bval)

  num_volumes <- nrow(bvec)
  if (length(bval) != num_volumes) {
    stop(sprintf("The number of b-values (%d) does not match the number of b-vectors (%d volumes).", length(bval), num_volumes))
  }

  if (!is.null(n_volumes)) {
    if (!is.numeric(n_volumes) || length(n_volumes) != 1L || is.na(n_volumes)) {
      stop("Parameter 'n_volumes' must be a single number or NULL.")
    }
    if (length(bval) != n_volumes) {
      stop(sprintf("The gradient table describes %d volumes, but the image has %d volumes.", length(bval), n_volumes))
    }
  }

  bzero_threshold <- .dti.bzero.threshold()

  # Handle missing values: a volume is dropped to b=0 if the information that is
  # missing cannot contradict that, otherwise the table is corrupt.
  num_missing_rows <- 0L
  for (row_idx in seq_len(num_volumes)) {
    missing_bval <- is.na(bval[row_idx])
    missing_bvec <- any(is.na(bvec[row_idx, ]))
    if (!missing_bval && !missing_bvec) {
      next
    }
    if (missing_bval && !missing_bvec && sum(bvec[row_idx, ]^2) > 0) {
      stop(sprintf("Corrupt gradient table: volume %d has a missing b-value but a valid gradient direction.", row_idx))
    }
    if (missing_bvec && !missing_bval && bval[row_idx] > 0) {
      stop(sprintf("Corrupt gradient table: volume %d has a missing gradient direction but a b-value of %g.", row_idx, bval[row_idx]))
    }
    bvec[row_idx, ] <- 0
    bval[row_idx] <- 0
    num_missing_rows <- num_missing_rows + 1L
  }
  if (num_missing_rows > 0L) {
    warning(sprintf(
      "%d volume(s) had missing (NA/NaN) values in the b-vectors and/or b-values and were interpreted as b=0 volumes.",
      num_missing_rows
    ))
  }

  if (any(is.infinite(bvec)) || any(is.infinite(bval))) {
    stop("The gradient table contains infinite values.")
  }

  # Report questionable entries. The data is deliberately returned unmodified:
  # renormalizing or rescaling here would silently change the b-values.
  bvec_norms <- sqrt(rowSums(bvec^2))
  dwi_rows <- which(bval > bzero_threshold)
  if (length(dwi_rows) > 0L) {
    deviating_rows <- dwi_rows[abs(bvec_norms[dwi_rows] - 1) > 0.01]
    if (length(deviating_rows) > 0L) {
      warning(sprintf(
        "%d of %d diffusion-weighted volumes have gradient vectors whose norm deviates from 1 by more than 1 percent (range %.4f to %.4f). Note that MRtrix3 rescales the b-value by the squared norm of the gradient vector in this situation; set the b-values accordingly if that is what you want.",
        length(deviating_rows), length(dwi_rows), min(bvec_norms[dwi_rows]), max(bvec_norms[dwi_rows])
      ))
    }
    zero_direction_rows <- dwi_rows[bvec_norms[dwi_rows] == 0]
    if (length(zero_direction_rows) > 0L) {
      warning(sprintf(
        "%d volume(s) have a b-value above %.1f s/mm^2 but a zero gradient direction, so the b-value cannot be trusted.",
        length(zero_direction_rows), bzero_threshold
      ))
    }
  }
  small_bvalue_rows <- which(bval > 0 & bval <= bzero_threshold & bvec_norms > 0)
  if (length(small_bvalue_rows) > 0L) {
    warning(sprintf(
      "%d volume(s) have a gradient direction but a b-value of at most %.1f s/mm^2. MRtrix3 clamps the b-value of such volumes to 0 when exporting to FSL format.",
      length(small_bvalue_rows), bzero_threshold
    ))
  }

  colnames(bvec) <- c("x", "y", "z")
  return(list("bvec" = bvec, "bval" = bval))
}
