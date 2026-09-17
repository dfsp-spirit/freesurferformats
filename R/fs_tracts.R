# Compact container for collections of tracts (streamlines) --------------------
#
# A tractogram is a collection of polylines that each have a variable number of
# points. The obvious R representation is a list with one matrix per tract, but
# that costs roughly 100 bytes of overhead per tract plus one separate
# allocation per tract. For whole-brain tractograms with millions of tracts
# this overhead dominates both memory and runtime.
#
# This class stores the same data the way 'nibabel' does in its ArraySequence
# class and the way the TRX file format stores it on disk: all coordinates in a
# single N x 3 matrix, plus an integer vector giving the number of points of
# each tract. Individual tracts are still available through `[[`, which slices
# the matrix, and `as.list()` converts to the plain old list-of-matrices
# representation if needed.


#' @title Create an fs.tracts instance from a compact tract representation.
#'
#' @description Creates the compact container used by the DTI tract readers and
#'   writers. Reading a track file with \code{\link{read.dti.tck}},
#'   \code{\link{read.dti.trk}} or \code{\link{read.dti.tsf}} returns instances of
#'   this class, and this function is the way to build one from your own data,
#'   e.g., to write a tractogram that was assembled or edited in R with
#'   \code{\link{write.dti.tck}} or \code{\link{write.dti.trk}}.
#'
#' @param coords numeric matrix with 3 columns, the concatenated coordinates of
#'   all tracts.
#'
#' @param lengths integer vector, the number of points of each tract. Must sum
#'   up to \code{nrow(coords)}.
#'
#' @param scalars numeric matrix or NULL. Per-point data, with one row per
#'   point (i.e., \code{nrow(scalars) == nrow(coords)}).
#'
#' @param properties numeric matrix or NULL. Per-tract data, with one row per
#'   tract.
#'
#' @param kind character string, either 'tck' or 'trk'. Determines what
#'   \code{[[} returns for a single tract.
#'
#' @return an \code{fs.tracts} instance.
#'
#' @examples
#' # Two tracts, the first with two points and the second with one.
#' coords <- matrix(c(0, 0, 0, 1, 1, 1, 5, 5, 5), ncol = 3, byrow = TRUE);
#' tracts <- fs.tracts(coords, lengths = c(2L, 1L));
#' length(tracts);
#' tracts[[1]];
#'
#' @export
fs.tracts <- function(coords, lengths, scalars = NULL, properties = NULL, kind = "tck") {
  coords <- as.matrix(coords)
  if (!is.numeric(coords) || ncol(coords) != 3L) {
    stop("Parameter 'coords' must be a numeric matrix with 3 columns.");
  }
  storage.mode(coords) <- "double";

  lengths <- as.integer(lengths);
  if (any(is.na(lengths)) || any(lengths < 0L)) {
    stop("Parameter 'lengths' must be a non-negative integer vector.");
  }
  if (sum(lengths) != nrow(coords)) {
    stop(sprintf("Inconsistent 'lengths': the lengths sum up to %d but 'coords' has %d rows.\n", sum(lengths), nrow(coords)));
  }

  if (!is.null(scalars)) {
    scalars <- as.matrix(scalars);
    if (nrow(scalars) != nrow(coords)) {
      stop(sprintf("Inconsistent 'scalars': %d rows, but 'coords' has %d rows.\n", nrow(scalars), nrow(coords)));
    }
  }

  if (!is.null(properties)) {
    properties <- as.matrix(properties);
    if (nrow(properties) != length(lengths)) {
      stop(sprintf("Inconsistent 'properties': %d rows, but there are %d tracts.\n", nrow(properties), length(lengths)));
    }
  }

  if (!kind %in% c("tck", "trk")) {
    stop("Parameter 'kind' must be one of 'tck' or 'trk'.");
  }

  # Offsets are stored explicitly instead of being recomputed with cumsum() on
  # every access, which would make per-tract indexing O(number of tracts).
  offsets <- c(0L, cumsum(lengths));

  ct <- list(
    coords = coords,
    lengths = lengths,
    offsets = offsets,
    scalars = scalars,
    properties = properties
  );
  return(structure(ct, class = "fs.tracts", kind = kind));
}


#' @title Check whether an object is an fs.tracts instance.
#'
#' @param x any R object.
#'
#' @return logical, TRUE if \code{x} is an \code{fs.tracts} instance.
#'
#' @examples
#' is.fs.tracts("not tracts");
#'
#' @export
is.fs.tracts <- function(x) {
  return(inherits(x, "fs.tracts"));
}


#' @title Get the concatenated coordinates of fs.tracts instances.
#'
#' @description Returns all tract coordinates as a single N x 3 matrix, with the
#'   tracts concatenated along the rows. This is the compact representation used
#'   internally, and the fastest way to access all coordinates, e.g., for
#'   plotting or for computing a bounding box.
#'
#' @param tracts an \code{fs.tracts} instance, as returned in the \code{tracks}
#'   entry of \code{read.dti.tck} or \code{read.dti.trk}.
#'
#' @return numeric matrix with 3 columns and one row per point of all tracts.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck");
#' coords <- fs.tracts.coords(tck$tracks);
#' bbox <- apply(coords, 2, range);
#' }
#'
#' @export
fs.tracts.coords <- function(tracts) {
  if (!is.fs.tracts(tracts)) {
    stop("Parameter 'tracts' must be an fs.tracts instance.");
  }
  return(tracts$coords);
}


#' @title Get the number of points of each tract.
#'
#' @description Returns one integer per tract, the number of points it consists
#'   of. The coordinates of tract \code{i} are the rows
#'   \code{(cumsum(c(1, lengths))[i]):(cumsum(lengths)[i])} of
#'   \code{fs.tracts.coords()}.
#'
#' @param tracts an \code{fs.tracts} instance, as returned in the \code{tracks}
#'   entry of \code{read.dti.tck} or \code{read.dti.trk}.
#'
#' @return integer vector with one entry per tract.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck");
#' lengths <- fs.tracts.lengths(tck$tracks);
#' mean(lengths);
#' }
#'
#' @export
fs.tracts.lengths <- function(tracts) {
  if (!is.fs.tracts(tracts)) {
    stop("Parameter 'tracts' must be an fs.tracts instance.");
  }
  return(tracts$lengths);
}


#' @title Get the number of tracts.
#'
#' @param tracts an \code{fs.tracts} instance.
#'
#' @return integer, the number of tracts.
#'
#' @keywords internal
fs.tracts.count <- function(tracts) {
  return(length(tracts$lengths));
}


#' @title Get the total number of points of all tracts.
#'
#' @param tracts an \code{fs.tracts} instance.
#'
#' @return integer, the total number of points.
#'
#' @keywords internal
fs.tracts.point.count <- function(tracts) {
  return(nrow(tracts$coords));
}


# S3 methods -------------------------------------------------------------------

#' @title Number of tracts in an fs.tracts instance.
#'
#' @param x an \code{fs.tracts} instance.
#'
#' @return integer, the number of tracts.
#'
#' @export
length.fs.tracts <- function(x) {
  return(length(x$lengths));
}


#' @title Access a single tract of an fs.tracts instance.
#'
#' @description For TCK data (see \code{\link{read.dti.tck}}), a tract is an n x 3
#'   numeric matrix of coordinates. For TRK data (see \code{\link{read.dti.trk}}), a
#'   tract is a named list with the entries \code{coords} (n x 3 matrix),
#'   \code{num_points} (integer), \code{scalars} (n x n_scalars matrix or NULL)
#'   and \code{properties} (numeric vector or NULL).
#'
#'   This returns a copy of the requested tract, the data is stored in a single
#'   matrix internally.
#'
#' @param x an \code{fs.tracts} instance.
#'
#' @param i positive integer, the index of the tract to retrieve.
#'
#' @return the tract, see the description.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck");
#' first_tract_coords <- tck$tracks[[1]];
#' }
#'
#' @export
`[[.fs.tracts` <- function(x, i) {
  ntracts <- length(x$lengths);

  if (length(i) != 1L || is.na(i)) {
    stop("Tract index must be a single non-NA integer value.");
  }
  i <- as.integer(i);
  if (i < 1L || i > ntracts) {
    stop(sprintf("Tract index %d is out of range, there are %d tracts.\n", i, ntracts));
  }

  start <- x$offsets[i];
  num_points <- x$lengths[i];
  rows <- if (num_points == 0L) integer(0L) else seq.int(start + 1L, start + num_points);
  coords <- x$coords[rows, , drop = FALSE];

  if (attr(x, "kind") == "tck") {
    return(coords);
  }

  scalars <- NULL;
  if (!is.null(x$scalars)) {
    scalars <- x$scalars[rows, , drop = FALSE];
  }
  properties <- NULL;
  if (!is.null(x$properties)) {
    properties <- as.vector(x$properties[i, ]);
  }
  return(list("scalars" = scalars, "properties" = properties, "coords" = coords, "num_points" = num_points));
}


#' @title Subset an fs.tracts instance.
#'
#' @param x an \code{fs.tracts} instance.
#'
#' @param i index vector (integer, numeric or logical), as usual in R.
#'
#' @return a new \code{fs.tracts} instance containing the selected tracts.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck");
#' first_ten <- tck$tracks[1:10];
#' }
#'
#' @export
`[.fs.tracts` <- function(x, i) {
  ntracts <- length(x$lengths);
  if (missing(i)) {
    sel <- seq_len(ntracts);
  } else {
    sel <- seq_len(ntracts)[i];
  }
  if (any(is.na(sel))) {
    stop("Cannot subset an fs.tracts instance with NA indices.");
  }

  lengths_sel <- x$lengths[sel];
  offsets_sel <- x$offsets[sel];

  # Row indices of the selected tracts, computed without looping over tracts.
  rows <- rep.int(offsets_sel, lengths_sel) + sequence(lengths_sel);

  scalars <- NULL;
  if (!is.null(x$scalars)) {
    scalars <- x$scalars[rows, , drop = FALSE];
  }
  properties <- NULL;
  if (!is.null(x$properties)) {
    properties <- x$properties[sel, , drop = FALSE];
  }

  return(fs.tracts(x$coords[rows, , drop = FALSE], lengths_sel,
                   scalars = scalars, properties = properties, kind = attr(x, "kind")));
}


#' @title Convert an fs.tracts instance to a plain list of tracts.
#'
#' @description Converts to the classic representation as a \code{list} with one
#'   entry per tract. This is convenient for interacting with code that expects
#'   a plain list, but note that it materializes one R object per tract and is
#'   therefore much more expensive in both memory and time. Prefer \code{[[}
#'   for accessing individual tracts.
#'
#' @param x an \code{fs.tracts} instance.
#'
#' @param ... ignored.
#'
#' @return list with one entry per tract.
#'
#' @examples
#' \dontrun{
#' tck <- read.dti.tck("brain.tck");
#' tracts_list <- as.list(tck$tracks);
#' }
#'
#' @export
as.list.fs.tracts <- function(x, ...) {
  return(lapply(seq_len(length(x$lengths)), function(idx) x[[idx]]));
}


#' @title Print an fs.tracts instance.
#'
#' @param x an \code{fs.tracts} instance.
#'
#' @param ... ignored.
#'
#' @return the instance, invisibly.
#'
#' @export
print.fs.tracts <- function(x, ...) {
  cat(sprintf("fs.tracts instance with %d tracts and %d points.\n", length(x$lengths), nrow(x$coords)));
  cat("Use [[ to access a single tract, [ to subset, as.list() for a plain list.\n");
  return(invisible(x));
}
