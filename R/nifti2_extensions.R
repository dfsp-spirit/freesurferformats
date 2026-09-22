# Functions for NIFTI v2 header extensions.
#
# A NIFTI-2 file consists of a fixed-size header (540 bytes), the header extension
# area, and the data (which starts at the file offset stored in the 'vox_offset'
# header field). The extension area starts with 4 bytes that indicate whether
# extensions follow: if the first of these bytes is non-zero, one or more
# extensions follow, otherwise the area is empty and 'vox_offset' is typically 544.
#
# Each extension consists of a 4-byte integer 'size' (the total size of the
# extension in bytes, including these 8 bytes of meta data, and always a multiple
# of 16), a 4-byte integer 'ecode' (the extension code, which identifies the type
# of the payload), and 'size - 8' bytes of payload. Trailing NUL bytes of the
# payload are padding only.
#
# The most prominent user of this mechanism is CIFTI2, which stores its XML
# metadata in a header extension with code 32. The implementation here is
# deliberately generic and does not interpret any payload.
#
# See https://nifti.nimh.nih.gov/nifti-2 for the standard, and
# https://www.nitrc.org/projects/cifti/ for CIFTI.


#' @title Create a NIFTI v2 header extension.
#'
#' @description NIFTI v2 files can store arbitrary data in header extensions, in the area between the header and the voxel data. This function creates such an extension. Pass the result to \code{\link{write.nifti2}}, which writes it to the file, and later read it back with \code{\link{read.nifti2.header}} plus \code{\link{nifti2.get.extension}}.
#'
#' @param ecode integer, the extension code. This identifies the type of the payload, but the code is not interpreted in any way. The CIFTI2 XML metadata uses code `32L`.
#'
#' @param content raw vector, character string, or `NULL`. The payload of the extension, i.e., the data to store. A character string is converted to UTF-8 bytes (several strings are joined with newline characters). Use `NULL` or omit for an empty payload.
#'
#' @return named list with the entries 'ecode' and 'content', representing one NIFTI v2 header extension.
#'
#' @seealso \code{\link{nifti2.get.extension}}, \code{\link{nifti2.extension.text}}
#'
#' @family nifti2 extensions
#'
#' @export
nifti2.extension <- function(ecode, content = NULL) {
  if (!is.numeric(ecode) || length(ecode) != 1L) {
    stop("Parameter 'ecode' must be a single integer.")
  }
  if (is.na(ecode) || ecode < 0L || ecode > .Machine$integer.max) {
    stop(sprintf("Parameter 'ecode' must be an integer in range 0 to %d, but it is %s.\n", .Machine$integer.max, as.character(ecode)))
  }
  ecode <- as.integer(ecode)

  if (is.null(content)) {
    content <- raw(0L)
  } else if (is.character(content)) {
    if (any(is.na(content))) {
      stop("Parameter 'content' must not contain NA values.")
    }
    content <- charToRaw(enc2utf8(paste(content, collapse = "\n")))
  } else if (!is.raw(content)) {
    stop("Parameter 'content' must be a raw vector, a character string, or NULL.")
  }

  return(list("ecode" = ecode, "content" = content))
}


#' @title Get a NIFTI v2 header extension by code.
#'
#' @description Retrieve one header extension from a NIFTI v2 header, by its extension code. See \code{\link{nifti2.extension}} for details on header extensions.
#'
#' @param niiheader named list, a NIFTI v2 header as returned by \code{\link{read.nifti2.header}} or created by \code{\link{ni2header.template}}.
#'
#' @param ecode integer, the extension code to search for. The CIFTI2 XML metadata uses code `32L`.
#'
#' @return the extension (a named list with entries 'ecode' and 'content', see \code{\link{nifti2.extension}}), or `NULL` if the header contains no extension with this code.
#'
#' @seealso \code{\link{nifti2.extension.text}}
#'
#' @family nifti2 extensions
#'
#' @export
nifti2.get.extension <- function(niiheader, ecode) {
  if (!is.list(niiheader)) {
    stop("Parameter 'niiheader' must be a named list, a NIFTI v2 header.")
  }
  if (is.null(niiheader$extensions) || length(niiheader$extensions) == 0L) {
    return(NULL)
  }
  ecodes <- vapply(niiheader$extensions, function(ext) {
    return(as.integer(ext$ecode))
  }, integer(1L))
  matching <- which(ecodes == as.integer(ecode))
  if (length(matching) == 0L) {
    return(NULL)
  }
  return(niiheader$extensions[[matching[1L]]])
}


#' @title Get the payload of a NIFTI v2 header extension as text.
#'
#' @description Convert the payload of a header extension to a character string. The trailing NUL bytes, which are only used to pad the extension to a multiple of 16 bytes, are removed. Note that embedded NUL bytes cannot be represented in an R string and are removed as well, so this function is only meaningful for text payloads (like the CIFTI2 XML metadata).
#'
#' @param extension a NIFTI v2 header extension, a named list with entries 'ecode' and 'content', see \code{\link{nifti2.extension}}.
#'
#' @return character string, the payload of the extension. The string is marked as UTF-8 encoded, which is the encoding required by the NIFTI standard.
#'
#' @family nifti2 extensions
#'
#' @export
nifti2.extension.text <- function(extension) {
  content <- nifti2.extension.content(extension, strip_nul = TRUE)
  text <- rawToChar(content)
  Encoding(text) <- "UTF-8"
  return(text)
}


#' @title Get the payload of a NIFTI v2 header extension as raw bytes.
#'
#' @inheritParams nifti2.extension.text
#'
#' @param strip_nul logical, whether to remove NUL bytes from the payload. This is required to convert the payload to a character string.
#'
#' @return a raw vector, the payload of the extension.
#'
#' @keywords internal
#' @family nifti2 extensions
nifti2.extension.content <- function(extension, strip_nul = TRUE) {
  if (!is.list(extension) || is.null(extension$content)) {
    stop("Parameter 'extension' must be a NIFTI v2 header extension as returned by nifti2.extension().")
  }
  content <- extension$content
  if (strip_nul) {
    content <- nifti2.strip.nul(content)
  }
  return(content)
}


#' @title Remove NUL bytes from a raw vector.
#'
#' @description Remove all NUL bytes from a raw vector. This is needed for the payload of NIFTI v2 header extensions, which are padded with NUL bytes, and for the payload of MATLAB files, but the function is not specific to any format.
#'
#' @param rawdata raw vector or integer vector with values in range 0 to 255.
#'
#' @return raw vector, the input without any NUL bytes.
#'
#' @keywords internal
nifti2.strip.nul <- function(rawdata) {
  if (is.integer(rawdata)) {
    rawdata <- as.raw(rawdata)
  }
  if (!is.raw(rawdata)) {
    stop("Parameter 'rawdata' must be a raw vector or an integer vector with values in range 0 to 255.") # nocov
  }
  rawdata <- rawdata[rawdata != as.raw(0L)]
  return(rawdata)
}


#' @title Compute the size of a NIFTI v2 header extension in a file.
#'
#' @description A NIFTI v2 header extension occupies a multiple of 16 bytes in the file, because the payload is padded with NUL bytes. See \code{\link{nifti2.extension}} for details.
#'
#' @inheritParams nifti2.extension.text
#'
#' @return integer, the size of the extension in bytes, as it is stored in the file. This includes the 8 bytes of meta data (the size and code fields) and the padding.
#'
#' @keywords internal
#' @family nifti2 extensions
nifti2.extension.size <- function(extension) {
  content <- nifti2.extension.content(extension, strip_nul = FALSE)
  # The total size (payload plus the 8 bytes for the 'size' and 'ecode' fields) must be a multiple of 16.
  return(as.integer((length(content) + 23L) %/% 16L * 16L))
}


#' @title Read the header extensions of a NIFTI v2 file.
#'
#' @param fh filehandle, a connection to a NIFTI v2 file, positioned at the first byte after the fixed-size header (i.e., at the extension flag bytes).
#'
#' @param available integer, the number of bytes available for header extensions, i.e., `vox_offset` minus the 544 bytes of the fixed-size header and the extension flag.
#'
#' @param endian character string, the endianness of the file, either 'little' or 'big'.
#'
#' @return list of header extensions, see \code{\link{nifti2.extension}}. Empty list if the file has no extensions.
#'
#' @keywords internal
nifti2.read.extensions <- function(fh, available, endian = "little") {
  extensions <- list()
  while (available >= 16L) {
    size <- readBin(fh, integer(), n = 1L, size = 4L, endian = endian)
    ecode <- readBin(fh, integer(), n = 1L, size = 4L, endian = endian)
    if (length(size) == 0L || is.na(size) || length(ecode) == 0L || is.na(ecode)) {
      warning("Truncated NIFTI v2 header extension: the file ends within the extension meta data. Ignoring the remaining bytes.\n")
      break
    }
    if (size == 0L) {
      # Zeroes, i.e., padding between the extensions and the data. This is not an extension.
      break
    }
    if (size < 16L) {
      warning(sprintf("Invalid NIFTI v2 header extension with extension code %d: the declared size %d is smaller than the 16 bytes of an extension. Ignoring the remaining bytes of the extension area.\n", ecode, size))
      break
    }
    if (size %% 16L != 0L) {
      warning(sprintf("The size (%d bytes) of the NIFTI v2 header extension with extension code %d is not a multiple of 16 bytes, which violates the NIFTI v2 standard. Reading it anyway.\n", size, ecode))
    }
    if (size > available) {
      warning(sprintf("Invalid NIFTI v2 header extension with extension code %d: the declared size %d is larger than the %d bytes available for header extensions. Ignoring the remaining bytes of the extension area.\n", ecode, size, available))
      break
    }
    content <- readBin(fh, raw(), n = size - 8L)
    if (length(content) != size - 8L) {
      warning(sprintf("Truncated NIFTI v2 header extension with extension code %d: expected %d bytes of payload, but the file ends after %d bytes. Ignoring the extension.\n", ecode, size - 8L, length(content)))
      break
    }
    # Trailing NUL bytes are padding only, they are not part of the payload.
    extensions[[length(extensions) + 1L]] <- nifti2.extension(ecode, nifti2.trailing.nul.removed(content))
    available <- available - size
  }
  return(extensions)
}


#' @title Write the header extensions of a NIFTI v2 file.
#'
#' @param fh filehandle, a connection to a NIFTI v2 file, positioned at the first byte after the fixed-size header (i.e., where the extension flag bytes go).
#'
#' @param extensions list of header extensions, see \code{\link{nifti2.extension}}. An empty list writes the extension flag for 'no extensions present'.
#'
#' @param endian character string, the endianness of the file, either 'little' or 'big'.
#'
#' @return integer, the number of bytes written (the extension flag bytes plus all extensions). The data must start at `544 + <return value>`.
#'
#' @keywords internal
nifti2.write.extensions <- function(fh, extensions, endian = "little") {
  if (length(extensions) == 0L) {
    writeBin(as.raw(c(0L, 0L, 0L, 0L)), fh, endian = endian)
    return(4L)
  }

  # The first byte of the 4 flag bytes is non-zero if extensions follow.
  writeBin(as.raw(c(1L, 0L, 0L, 0L)), fh, endian = endian)
  num_written <- 4L

  for (ext in extensions) {
    size <- nifti2.extension.size(ext)
    writeBin(as.integer(size), fh, size = 4L, endian = endian)
    writeBin(as.integer(ext$ecode), fh, size = 4L, endian = endian)
    num_written <- num_written + 8L
    content <- nifti2.extension.content(ext, strip_nul = FALSE)
    if (length(content) > 0L) {
      writeBin(content, fh, endian = endian)
      num_written <- num_written + length(content)
    }
    # Zero padding up to the next multiple of 16 bytes.
    num_pad <- size - 8L - length(content)
    if (num_pad > 0L) {
      writeBin(as.raw(rep(0L, num_pad)), fh, endian = endian)
      num_written <- num_written + num_pad
    }
  }

  return(as.integer(num_written))
}


#' @title Remove trailing NUL bytes from a raw vector.
#'
#' @description Remove the NUL bytes at the end of a raw vector, but keep any NUL bytes before them. This is needed for the payload of NIFTI v2 header extensions, which is padded with NUL bytes to a multiple of 16 bytes, but can in theory contain NUL bytes as part of the payload.
#'
#' @inheritParams nifti2.strip.nul
#'
#' @return raw vector, the input without trailing NUL bytes.
#'
#' @keywords internal
nifti2.trailing.nul.removed <- function(rawdata) {
  if (is.integer(rawdata)) {
    rawdata <- as.raw(rawdata)
  }
  if (!is.raw(rawdata)) {
    stop("Parameter 'rawdata' must be a raw vector or an integer vector with values in range 0 to 255.") # nocov
  }
  num_bytes <- length(rawdata)
  while (num_bytes > 0L && rawdata[num_bytes] == as.raw(0L)) {
    num_bytes <- num_bytes - 1L
  }
  if (num_bytes == 0L) {
    return(raw(0L))
  }
  return(rawdata[seq_len(num_bytes)])
}
