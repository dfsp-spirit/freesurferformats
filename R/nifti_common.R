# Common function used for reading/writing NIFTI v1 and/or v2 files

#' @title Perform basic sanity checks on NIFTI header data. These are in no way meant to be exhaustive.
#'
#' @param niiheader named list, the NIFTI header.
#'
#' @param nifti_version integer, one of 1L or 2L. The NIFTI format version.
#'
#' @return logical, whether the check was okay
#'
#' @export
nifti.header.check <- function(niiheader, nifti_version = 1L) {
  nifti_version <- as.integer(nifti_version)
  if (!nifti_version %in% c(1L, 2L)) {
    stop("Parameter 'nifti_version' must be 1L or 2L.")
  }

  is_okay_l8 <- nifti.field.check.length(niiheader, c("dim", "pix_dim"), 8L)
  is_okay_l4 <- nifti.field.check.length(niiheader, c("srow_x", "srow_y", "srow_z"), 4L)
  is_okay_l1 <- nifti.field.check.length(niiheader, c("qform_code", "sform_code", "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y", "qoffset_z", "sizeof_hdr", "intent_p1", "intent_p2", "intent_p3", "cal_max", "cal_min", "xyzt_units", "slice_start", "slice_end", "slice_duration", "toffset", "slice_code", "scl_slope", "scl_inter", "datatype", "bitpix", "intent_code", "vox_offset"), 1L)

  is_okay_char_descrip <- nifti.field.check.nchar.max(niiheader, c("descrip"), 80L)
  is_okay_char_aux_file <- nifti.field.check.nchar.max(niiheader, c("aux_file"), 24L)
  is_okay_char_intent_name <- nifti.field.check.nchar.max(niiheader, c("intent_name"), 16L)


  is_okay_nifti1_specific_fields <- TRUE
  is_okay_nifti2_specific_fields <- TRUE
  if (nifti_version == 1L) {
    is_okay_nifti1_l1 <- nifti.field.check.length(niiheader, c("glmax", "glmin"), 1L)
    is_okay_nifti1_char_magic <- nifti.field.check.nchar.max(niiheader, c("magic"), 4L)
    is_okay_nifti1_specific_fields <- (is_okay_nifti1_l1 & is_okay_nifti1_char_magic)
  } else {
    is_okay_nifti2_char_magic <- nifti.field.check.nchar.max(niiheader, c("magic"), 8L)
    is_okay_nifti2_specific_fields <- is_okay_nifti2_char_magic
  }

  is_okay <- (is_okay_l8 & is_okay_l4 & is_okay_l1 & is_okay_char_descrip & is_okay_char_aux_file & is_okay_char_intent_name & is_okay_nifti1_specific_fields & is_okay_nifti2_specific_fields)
  return(is_okay)
}


#' @title Check whether vector has expected length.
#'
#' @param niiheader named list, representing a NIFTI v1 or v2 header
#'
#' @param fields vector of character string, the field names to check
#'
#' @param dlength integer, the expected length of all fields
#'
#' @return logical, whether the checks were okay
#'
#' @keywords internal
nifti.field.check.length <- function(niiheader, fields, dlength) {
  is_okay <- TRUE
  for (f in fields) {
    if (length(niiheader[[f]]) != dlength) {
      message(sprintf("Invalid '%s' field length: expected %d, found %d.\n", f, dlength, length(niiheader[[f]])))
      is_okay <- FALSE
    }
  }
  return(is_okay)
}


#' @title Check whether character string fields have less than or equal to expected length.
#'
#' @param niiheader named list, representing a NIFTI v1 or v2 header
#'
#' @param fields vector of character string, the field names to check
#'
#' @param dlength integer, the max length of all fields
#'
#' @return logical, whether the checks were okay
#'
#' @keywords internal
nifti.field.check.nchar.max <- function(niiheader, fields, dlength) {
  is_okay <- TRUE
  for (f in fields) {
    if (is.character(niiheader[[f]])) {
      if (nchar(niiheader[[f]]) > dlength) {
        message(sprintf("Invalid '%s' field length: expected <= %d chars, found %d.\n", f, dlength, nchar(niiheader[[f]])))
        is_okay <- FALSE
      }
    } else {
      message(sprintf("Invalid '%s' field type: expected type 'character'.\n", f))
      is_okay <- FALSE
    }
  }
  return(is_okay)
}


#' @title Compute data dimensions from the 'dim' field of the NIFTI (v1 or v2) header.
#'
#' @param dimfield integer vector of length 8, the `dim` field of a NIFTI v1 or v2 header, as returned by \code{\link{read.nifti2.header}} or \code{\link{read.nifti1.header}}.
#'
#' @return integer vector of length <= 7. The lengths of the used data dimensions. The 'dim' field always has length 8, and the first entry is the number of actually used dimensions. The return value is constructed by stripping the first field and returning the used fields.
#'
#' @examples
#' nifti.datadim.from.dimfield(c(3, 256, 256, 256, 1, 1, 1, 1))
#'
#' @family NIFTI helper functions
#'
#' @export
nifti.datadim.from.dimfield <- function(dimfield) {
  if (length(dimfield) != 8L) {
    stop(sprintf("Invalid 'dimfield' parameter: must be integer vector of length 8, found length %d: '%s'.\n", length(dimfield), paste(dimfield, collapse = " ")))
  }
  num_dim <- dimfield[1]
  if (num_dim == 1L) {
    return(dimfield[2])
  }
  return(dimfield[2:(num_dim + 1L)])
}


#' @title Compute NIFTI dim field for data dimension.
#'
#' @param datadim integer vector, the result of calling `dim` on your data. The length must be <= 7.
#'
#' @return NIFTI header `dim` field, an integer vector of length 8
#'
#' @examples
#' nifti.datadim.to.dimfield(c(256, 256, 256))
#'
#' @family NIFTI helper functions
#'
#' @export
nifti.datadim.to.dimfield <- function(datadim) {
  dim_field <- rep(1L, 8L)
  ndim <- length(datadim)
  if (ndim > 7L) {
    stop(sprintf("Length of datadim must be <= 7, but is %d. Not supported by NIFTI format, please reshape.\n", ndim))
  }
  dim_field[1] <- ndim
  dim_field[2:(2 + ndim - 1L)] <- datadim
  return(dim_field)
}


# Copyright notice for the next function 'read.fixed.char.binary', which was taken from the '.readCharWithEmbeddedNuls' function in package 'oro.nifti'.
# I only changed the coding style.
#
## Copyright (c) 2009-2014 Brandon Whitcher and Volker Schmid
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
##
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer.
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#' @title Read fixed length char, possibly containing embedded zeroes, from binary file.
#'
#' @author The original version was written by Brandon Whitcher and Volker Schmid. See the source for the full legal info. The coding style was adapted to freesurferformats and the docs were added by Tim Schäfer.
#'
#' @param filehandle connection to read.
#'
#' @param n the number of bytes to read.
#'
#' @param to the target character encoding.
#'
#' @return the string in the target encoding, with the embedded zeroes removed.
#'
#' @keywords internal
read.fixed.char.binary <- function(filehandle, n, to = "UTF-8") {
  txt <- readBin(filehandle, "raw", n)
  return(iconv(rawToChar(txt[txt != as.raw(0)]), to = to))
}


#' @title Compute the voxel-to-RAS matrix from the geometry fields of a NIFTI v1 header.
#'
#' @description The NIFTI v1 header can store up to two descriptions of the mapping from voxel indices to world
#'   coordinates: the `sform` (a full affine matrix in the `srow_x`, `srow_y` and `srow_z` fields) and the `qform` (a
#'   rotation, given as a quaternion, plus a translation, given in the `quoffset` fields). Each of them is only valid
#'   if the corresponding code field is not zero, and the `sform` takes precedence over the `qform` if both are
#'   present. This is the same rule that the NIFTI standard defines and that other implementations (`nibabel`,
#'   `oro.nifti`) follow.
#'
#' @param niiheader named list, a NIFTI v1 header as returned by \code{\link{read.nifti1.header}}.
#'
#' @return a 4x4 numeric matrix (the voxel-to-RAS transformation), or `NULL` if the header contains neither an
#'   `sform` nor a `qform`. The matrix implements the NIFTI convention that the rotation of the `qform` applies to
#'   the *left* of the scaled voxel axes and that the third axis is flipped if the `qfac` field (`pix_dim[1]`) is
#'   negative.
#'
#' @note This is a header based re-implementation of the geometry computation of \code{\link{read.fs.volume.nii}},
#'   which reads an `oro.nifti` instance. The two are compared against each other in the unit tests, and against
#'   `nibabel` in `dev_tools/check_analyze_conversion.R`.
#'
#' @keywords internal
nifti.header.to.vox2ras <- function(niiheader) {
  if (!is.null(niiheader$sform_code) && niiheader$sform_code != 0L) {
    return(rbind(niiheader$srow_x, niiheader$srow_y, niiheader$srow_z, c(0., 0., 0., 1.)))
  }

  if (is.null(niiheader$qform_code) || niiheader$qform_code == 0L) {
    return(NULL) # Neither an sform nor a qform, the orientation is unknown.
  }

  qb <- niiheader$quatern_b
  qc <- niiheader$quatern_c
  qd <- niiheader$quatern_d

  qa <- 1.0 - (qb * qb + qc * qc + qd * qd)
  if (qa < 1.0e-7) {
    qa <- 1.0 / sqrt(qb * qb + qc * qc + qd * qd)
    qb <- qa * qb
    qc <- qa * qc
    qd <- qa * qd
    qa <- 0.0
  } else {
    qa <- sqrt(qa)
  }

  # The rotation matrix of the quaternion, see the 'quaternion' section of the NIFTI v1 specification.
  rot_mat <- matrix(rep(0., 9L), nrow = 3L)
  rot_mat[1, 1] <- 1.0 - 2.0 * (qc * qc + qd * qd)
  rot_mat[1, 2] <- 2.0 * (qb * qc - qa * qd)
  rot_mat[1, 3] <- 2.0 * (qb * qd + qa * qc)
  rot_mat[2, 1] <- 2.0 * (qb * qc + qa * qd)
  rot_mat[2, 2] <- 1.0 - 2.0 * (qb * qb + qd * qd)
  rot_mat[2, 3] <- 2.0 * (qc * qd - qa * qb)
  rot_mat[3, 1] <- 2.0 * (qb * qd - qa * qc)
  rot_mat[3, 2] <- 2.0 * (qc * qd + qa * qb)
  rot_mat[3, 3] <- 1.0 - 2.0 * (qb * qb + qc * qc)

  qfac <- niiheader$pix_dim[1]
  if (qfac == 0.) {
    qfac <- 1.
  }
  if (!(qfac == -1. || qfac == 1.)) {
    stop(sprintf("Invalid 'qfac' value %.4f in the NIFTI v1 header, expected 0, 1 or -1.\n", qfac))
  }

  vox2ras <- diag(4L)
  vox2ras[1:3, 1:3] <- rot_mat %*% diag(c(niiheader$pix_dim[2], niiheader$pix_dim[3], niiheader$pix_dim[4] * qfac))
  vox2ras[1:3, 4L] <- c(niiheader$qoffset_x, niiheader$qoffset_y, niiheader$qoffset_z)

  return(vox2ras)
}


#' @title Read raw voxel values of a NIFTI v1/v2 or ANALYZE file from a connection.
#'
#' @description Read `num_values` voxel values of the data type described by the `datatype` and `bitpix` fields of
#'   a header, in the endianness of the file. This is the shared low level reading code of the NIFTI and ANALYZE
#'   readers.
#'
#' @param fh connection to read from, positioned at the first value.
#'
#' @param datatype integer, the `datatype` header field.
#'
#' @param bitpix integer, the `bitpix` header field.
#'
#' @param num_values integer, the number of values to read.
#'
#' @param endian character string, the endianness of the file, 'little' or 'big'.
#'
#' @return numeric or integer vector of length `num_values`, the raw values as they are stored in the file.
#'
#' @note The signedness of the data type is taken from \code{\link{nifti.dtype.info}}: an unsigned 8 bit value of
#'   200 is returned as 200, not as -56. R's `readBin` reads integers as signed by default, and it silently ignores
#'   a `signed` argument for 4 byte integers, so the three unsigned types need special care.
#'
#' @keywords internal
read.nifti.values <- function(fh, datatype, bitpix, num_values, endian) {
  type_info <- nifti.dtype.info(datatype, bitpix)

  if (type_info$is_float) {
    return(read_safe_bin(fh, numeric(), n = num_values, size = type_info$size, endian = endian))
  }

  if (type_info$size == 4L) {
    values <- read_safe_bin(fh, integer(), n = num_values, size = 4L, endian = endian)
    if (!type_info$signed) {
      # R has no unsigned 32 bit integer type, so values above 2^31-1 are returned as negative integers by readBin
      # and have to be converted back by hand. They are returned as doubles, since integers cannot represent them.
      values <- as.double(values)
      negative <- values < 0.
      if (any(negative)) {
        values[negative] <- values[negative] + 2^32
      }
    }
    return(values)
  }

  return(read_safe_bin(fh, integer(), n = num_values, size = type_info$size, endian = endian, signed = type_info$signed))
}
