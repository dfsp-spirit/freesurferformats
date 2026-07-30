# Functions for memory safety and security when reading binary neuroimaging files.
# These protect against malicious or corrupt files causing Denial-of-Service via
# excessive memory allocation, integer overflow, or truncated reads.


#' @title Get the configured maximum allocation size in bytes.
#'
#' @description Returns the maximum number of bytes that the package is allowed
#'   to allocate when reading binary data payloads. The limit is resolved in
#'   this order: (1) environment variable \code{FREESURFERFORMATS_MAX_ALLOC_BYTES},
#'   (2) R option \code{freesurferformats.max_alloc_bytes}, (3) the hard-coded
#'   default of 2 GB. Set the limit to \code{Inf} to disable the check.
#'
#' @return a single numeric value, the maximum allocation size in bytes.
#'
#' @keywords internal
get_max_alloc_bytes <- function() {
  # 1. Check environment variable
  env_val <- Sys.getenv("FREESURFERFORMATS_MAX_ALLOC_BYTES", unset = NA_character_)
  if (!is.na(env_val)) {
    val <- suppressWarnings(as.numeric(env_val))
    if (!is.na(val) && val > 0) {
      return(val)
    }
    warning(sprintf("Invalid FREESURFERFORMATS_MAX_ALLOC_BYTES='%s', falling back to R option or default.", env_val))
  }

  # 2. Check R option
  opt_val <- getOption("freesurferformats.max_alloc_bytes", default = NA_real_)
  if (!is.na(opt_val) && is.numeric(opt_val) && opt_val > 0) {
    return(opt_val)
  }

  # 3. Hard-coded default: 2 GB
  2e9
}


#' @title Validate that a requested allocation does not exceed the safety limit.
#'
#' @description Given dimension sizes and bytes per element, checks that the
#'   total allocation size is safe. Catches negative/NA/Inf dimensions, integer
#'   overflow (by converting to double), and enforces the max allocation limit.
#'
#' @param dims numeric vector of dimension sizes (e.g., \code{c(256, 256, 256)}).
#'
#' @param bytes_per_elem single numeric value, the number of bytes per element
#'   (e.g., \code{4} for float32).
#'
#' @param max_bytes single numeric value, the maximum allowed allocation in bytes.
#'   Defaults to the result of \code{get_max_alloc_bytes()}. Pass \code{Inf} to
#'   disable the limit check (negative/NA/Inf dims are still rejected).
#'
#' @return the total number of elements (as double), invisibly. The function
#'   stops with an error if the allocation would be unsafe.
#'
#' @keywords internal
validate_allocation_size <- function(dims, bytes_per_elem, max_bytes = get_max_alloc_bytes()) {
  if (!is.numeric(dims)) {
    stop("validate_allocation_size: 'dims' must be numeric.")
  }
  if (any(is.na(dims)) || any(dims < 0) || any(is.infinite(dims))) {
    stop(sprintf(
      "Invalid dimension value(s) in header: %s. Dimensions must be finite and non-negative.",
      paste(dims, collapse = ", ")
    ))
  }

  # Use double precision to avoid 32-bit integer overflow
  total_elements <- prod(as.numeric(dims))

  if (is.na(total_elements) || is.infinite(total_elements)) {
    stop(sprintf(
      "Dimension calculation overflow for dims (%s): result is NA or Inf.",
      paste(dims, collapse = ", ")
    ))
  }

  total_bytes <- total_elements * as.numeric(bytes_per_elem)

  if (is.finite(max_bytes) && total_bytes > max_bytes) {
    stop(sprintf(
      "Requested allocation of %.1f MB exceeds the safety limit of %.1f MB.\n",
      total_bytes / 1e6, max_bytes / 1e6
    ), "Set a higher limit with: options(freesurferformats.max_alloc_bytes = <bytes>)\n",
    "  or set environment variable: FREESURFERFORMATS_MAX_ALLOC_BYTES=<bytes>\n",
    "  Use Inf to disable the limit.")
  }

  invisible(total_elements)
}


#' @title Check that a file is large enough to contain the expected data payload.
#'
#' @description Compares the expected data size against the file size on disk.
#'   Stops with an error if the file is too small (truncated or corrupt).
#'
#' @param filepath character string, path to the file.
#'
#' @param header_bytes single numeric value, the number of bytes consumed by
#'   the file header (everything before the data payload).
#'
#' @param data_bytes single numeric value, the expected number of bytes in the
#'   data payload.
#'
#' @return logical, \code{TRUE} (invisibly) if the file is large enough.
#'   Stops with an error otherwise.
#'
#' @keywords internal
check_file_size <- function(filepath, header_bytes, data_bytes) {
  if (!file.exists(filepath)) {
    stop(sprintf("File '%s' does not exist or cannot be read.", filepath))
  }

  fs <- file.info(filepath)$size
  if (is.na(fs)) {
    stop(sprintf("Could not determine file size for '%s'.", filepath))
  }

  required <- as.numeric(header_bytes) + as.numeric(data_bytes)

  if (is.na(required) || is.infinite(required)) {
    stop(sprintf("Invalid header+data size calculation for '%s': header=%s, data=%s.",
                 filepath, header_bytes, data_bytes))
  }

  if (fs < required) {
    stop(sprintf(
      "File '%s' is too small: size on disk is %.1f MB, but header+data requires %.1f MB. File may be truncated or corrupt.",
      filepath, fs / 1e6, required / 1e6
    ))
  }

  invisible(TRUE)
}


#' @title Safe wrapper around readBin that verifies the number of elements read.
#'
#' @description Calls \code{\link[base]{readBin}} and then checks that the
#'   returned vector has the expected length. If the file ends prematurely,
#'   \code{readBin} silently returns fewer elements — this wrapper turns that
#'   into an explicit error.
#'
#' @param con a connection object.
#'
#' @param what character string or type, passed to \code{readBin}.
#'
#' @param n integer, the number of elements to read.
#'
#' @param size integer, the number of bytes per element.
#'
#' @param endian character string, \code{"big"} or \code{"little"}.
#'
#' @param ... additional arguments passed to \code{readBin}.
#'
#' @return the vector of data read from the connection.
#'
#' @keywords internal
read_safe_bin <- function(con, what, n, size, endian, ...) {
  data <- readBin(con = con, what = what, n = n, size = size, endian = endian, ...)
  if (length(data) != n) {
    stop(sprintf(
      "Truncated file: expected %d elements but only read %d. The file may be corrupt or incomplete.",
      n, length(data)
    ))
  }
  return(data)
}


#' @title Check that a numeric vector or matrix contains only finite values.
#'
#' @description Stops with an error if the input contains \code{NA}, \code{NaN},
#'   or \code{Inf} values.
#'
#' @param x a numeric vector or matrix.
#'
#' @param label character string, a human-readable label for the data (used in
#'   the error message).
#'
#' @return \code{TRUE} (invisibly) if all values are finite. Stops otherwise.
#'
#' @keywords internal
check_all_finite <- function(x, label = "data") {
  if (!all(is.finite(x))) {
    nbad <- sum(!is.finite(x))
    stop(sprintf(
      "Found %d non-finite value(s) (NA, NaN, or Inf) in %s. The file may be corrupt.",
      nbad, label
    ))
  }
  invisible(TRUE)
}
