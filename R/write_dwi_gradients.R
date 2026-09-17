# Writers for diffusion MRI gradient tables (b-vectors / b-values) in FSL
# bvecs/bvals format and in MRtrix3 gradient table format. See
# read_dwi_gradients.R for the references that define these formats.


#' @title Format numeric values for a gradient table text file.
#'
#' @description Writes gradient values with enough significant digits to
#'   survive a round trip, in a plain decimal or scientific notation that the
#'   FSL, MRtrix3, DIPY and NumPy readers all accept. Note that
#'   \code{formatC(..., digits = 15)} cannot be used here: with the default
#'   \code{width = NULL} it treats \code{digits} as the field width and pads
#'   every value with spaces, which makes the files hard to read.
#'
#' @param x numeric vector, the values to format.
#'
#' @return character vector of the same length as `x`.
#'
#' @keywords internal
.format.gradient.values <- function(x) {
  x[x == 0] <- 0 # normalize negative zero, so that it does not show up as '-0'
  return(vapply(as.double(x), function(value) sprintf("%.15g", value), character(1L), USE.NAMES = FALSE))
}


#' @title Write a numeric table to a text file, one matrix row per line.
#'
#' @param filepath character string, path to the output file. Files ending in
#'   `.gz` are gzip-compressed.
#'
#' @param m numeric matrix, the table to write.
#'
#' @return NULL, invisibly.
#'
#' @keywords internal
.write.numeric.table <- function(filepath, m) {
  if (!is.matrix(m)) {
    m <- matrix(m, nrow = 1L)
  }
  if (!is.numeric(m)) {
    stop("The table to write must be numeric.")
  }

  formatted <- matrix(.format.gradient.values(as.vector(m)), nrow = nrow(m), ncol = ncol(m))
  lines <- apply(formatted, 1L, paste, collapse = " ")

  con <- if (filepath.ends.with(filepath, c(".gz"))) gzfile(filepath, "wt") else file(filepath, "wt")
  on.exit(
    {
      close(con)
    },
    add = TRUE
  )
  writeLines(lines, con)

  return(invisible(NULL))
}


#' @title Write b-values to a FSL-style bvals file.
#'
#' @description Writes the diffusion weighting of each volume of a diffusion
#'   MRI dataset to a plain text file, which is the *bvals* part of the FSL
#'   bvecs/bvals format.
#'
#' @param filepath character string, path to the output file. The conventional
#'   extension is `.bval` or `.bvals`, but that is not enforced. Files ending in
#'   `.gz` are gzip-compressed.
#'
#' @param bval numeric vector, the b-value of each volume, in s/mm^2.
#'
#' @param layout character string, one of 'components' or 'volumes'.
#'   `components` (the default) writes all values into a single line, which is
#'   what the FSL tools produce, while `volumes` writes one value per line,
#'   which is what the Human Connectome Project distributes.
#'
#' @return NULL, invisibly. Called for the side effect of writing the file.
#'
#' @family dti functions
#'
#' @export
write.dti.bval <- function(filepath, bval, layout = "components") {
  if (!(layout %in% c("components", "volumes"))) {
    stop("Parameter 'layout' must be one of 'components' or 'volumes'.")
  }
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

  if (layout == "components") {
    return(.write.numeric.table(filepath, matrix(bval, nrow = 1L)))
  }
  return(.write.numeric.table(filepath, matrix(bval, ncol = 1L)))
}


#' @title Write b-vectors to a FSL-style bvecs file.
#'
#' @description Writes the diffusion gradient directions of a diffusion MRI
#'   dataset to a plain text file, which is the *bvecs* part of the FSL
#'   bvecs/bvals format. Note that these vectors are interpreted relative to the
#'   *image* axes, so the file is only meaningful together with the image it was
#'   derived from.
#'
#' @param filepath character string, path to the output file. The conventional
#'   extension is `.bvec` or `.bvecs`, but that is not enforced. Files ending in
#'   `.gz` are gzip-compressed.
#'
#' @param bvec numeric matrix with 3 columns (one row per volume) or 3 rows (one
#'   column per volume), the gradient direction of each volume.
#'
#' @param layout character string, one of 'components' or 'volumes'.
#'   `components` (the default) writes the file with 3 lines, one per vector
#'   component, which is the FSL layout, while `volumes` writes one volume per
#'   line, which is what the Human Connectome Project distributes.
#'
#' @return NULL, invisibly. Called for the side effect of writing the file.
#'
#' @note A square 3x3 input is ambiguous. Since a matrix passed in memory
#'   follows the R convention of one row per volume, the rows are written as the
#'   volumes here; \code{\link[freesurferformats]{read.dti.bvec}} assumes the
#'   components for a square file, so reading such a file back yields the
#'   transposed matrix and warns about the ambiguity.
#'
#' @family dti functions
#'
#' @export
write.dti.bvec <- function(filepath, bvec, layout = "components") {
  if (!(layout %in% c("components", "volumes"))) {
    stop("Parameter 'layout' must be one of 'components' or 'volumes'.")
  }
  if (is.data.frame(bvec)) {
    bvec <- as.matrix(bvec)
  }
  if (!is.matrix(bvec) || !is.numeric(bvec)) {
    stop("Parameter 'bvec' must be a numeric matrix, with one row per volume (or one column per volume) and 3 columns.")
  }
  bvec <- .canonicalize.gradient.table(bvec, 3L, layout = "auto", what = "b-vector")

  if (layout == "components") {
    return(.write.numeric.table(filepath, t(bvec)))
  }
  return(.write.numeric.table(filepath, bvec))
}


#' @title Write a gradient table in MRtrix3 format.
#'
#' @description Writes a diffusion gradient table to a plain text file with one
#'   line per DWI volume, each line holding the four space-separated values
#'   `[ x y z b ]`. This is the format that the `-grad` option of the MRtrix3
#'   commands expects, and the format of the `dw_scheme` entry in the header of
#'   a `.mif` image.
#'
#' @param filepath character string, path to the output file. Files ending in
#'   `.gz` are gzip-compressed.
#'
#' @param bvec numeric matrix with 3 columns (one row per volume) or 3 rows (one
#'   column per volume), the gradient directions. Alternatively a full gradient
#'   table, i.e. a numeric matrix with 4 columns named or ordered 'x', 'y', 'z'
#'   and 'b', as returned by
#'   \code{\link[freesurferformats]{read.dti.grad}}. If a full table is given,
#'   the `bval` parameter must be left at `NULL`.
#'
#' @param bval numeric vector, the b-value of each volume, in s/mm^2. Ignored if
#'   `bvec` is a full gradient table.
#'
#' @param layout character string, one of 'volumes' or 'components'. `volumes`
#'   (the default) writes one volume per line, which is the MRtrix layout, while
#'   `components` writes the transposed variant with 4 lines.
#'
#' @return NULL, invisibly. Called for the side effect of writing the file.
#'
#' @family dti functions
#'
#' @export
write.dti.grad <- function(filepath, bvec, bval = NULL, layout = "volumes") {
  if (!(layout %in% c("components", "volumes"))) {
    stop("Parameter 'layout' must be one of 'components' or 'volumes'.")
  }
  if (is.data.frame(bvec)) {
    bvec <- as.matrix(bvec)
  }

  if (is.null(bval)) {
    if (!is.matrix(bvec) || !is.numeric(bvec)) {
      stop("If 'bval' is not given, parameter 'bvec' must be a full gradient table, i.e. a numeric matrix with 4 columns (or 4 rows).")
    }
    gt <- .canonicalize.gradient.table(bvec, 4L, layout = "auto", what = "gradient table")
  } else {
    if (!is.matrix(bvec) || !is.numeric(bvec)) {
      stop("Parameter 'bvec' must be a numeric matrix, with one row per volume (or one column per volume) and 3 columns.")
    }
    gradients <- validate.dti.gradients(bvec, bval)
    gt <- cbind(gradients$bvec, gradients$bval)
  }

  gt <- .name.gradient.table.columns(gt)
  if (layout == "components") {
    return(.write.numeric.table(filepath, t(gt)))
  }
  return(.write.numeric.table(filepath, gt))
}
