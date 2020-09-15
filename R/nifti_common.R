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
  nifti_version = as.integer(nifti_version);
  if(! nifti_version %in% c(1L, 2L)) {
    stop("Parameter 'nifti_version' must be 1L or 2L.");
  }

  is_okay_l8 = nifti.field.check.length(niiheader, c('dim', 'pix_dim'), 8L);
  is_okay_l4 = nifti.field.check.length(niiheader, c('srow_x', 'srow_y', 'srow_z'), 4L);
  is_okay_l1 = nifti.field.check.length(niiheader, c('qform_code', 'sform_code', 'quatern_b', 'quatern_c', 'quatern_d', 'qoffset_x', 'qoffset_y', 'qoffset_z', 'sizeof_hdr', 'intent_p1', 'intent_p2', 'intent_p3', 'cal_max', 'cal_min', 'xyzt_units', 'slice_start', 'slice_end', 'slice_duration', 'toffset', 'slice_code', 'scl_slope', 'scl_inter', 'datatype', 'bitpix', 'intent_code', 'vox_offset'), 1L);

  is_okay_nifti1_specific_fields = TRUE;
  if(nifti_version == 1L) {
    is_okay_nifti1_specific_fields = nifti.field.check.length(niiheader, c('glmax', 'glmin'), 1L);
  }

  is_okay = (is_okay_l8 & is_okay_l4 & is_okay_l1 & is_okay_nifti1_specific_fields);
  return(is_okay);
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
  is_okay = TRUE;
  for(f in fields) {
    if(length(niiheader[[f]]) != dlength) {
      message(sprintf("Invalid '%s' field length: expected %d, found %d.\n", f, dlength, length(niiheader[[f]])));
      is_okay = FALSE;
    }
  }
  return(is_okay);
}

#' @title Compute data dimensions from the 'dim' field of the NIFTI (v1 or v2) header.
#'
#' @param dimfield integer vector of length 8, the `dim` field of a NIFTI v1 or v2 header, as returned by \code{\link{read.nifti2.header}} or \code{\link{read.nifti1.header}}.
#'
#' @return integer vector of length <= 7. The lengths of the used data dimensions. The 'dim' field always has length 8, and the first entry is the number of actually used dimensions. The return value is constructed by stripping the first field and returning the used fields.
#'
#' @examples
#'    nifti.datadim.from.dimfield(c(3, 256, 256, 256, 1, 1, 1, 1));
#'
#' @family NIFTI helper functions
#'
#' @export
nifti.datadim.from.dimfield <- function(dimfield) {
  if(length(dimfield) != 8L) {
    stop(sprintf("Invalid 'dimfield' parameter: must be integer vector of length 8, found length %d: '%s'.\n", length(dimfield), paste(dimfield, collapse=" ")));
  }
  num_dim = dimfield[1];
  if(num_dim == 1L) {
    return(dimfield[2]);
  }
  return(dimfield[2:(num_dim + 1L)]);
}


#' @title Compute NIFTI dim field for data dimension.
#'
#' @param datadim integer vector, the result of calling `dim` on your data. The length must be <= 7.
#'
#' @return NIFTI header `dim` field, an integer vector of length 8
#'
#' @examples
#'    nifti.datadim.to.dimfield(c(256, 256, 256));
#'
#' @family NIFTI helper functions
#'
#' @export
nifti.datadim.to.dimfield <- function(datadim) {
  dim_field = rep(1L, 8L);
  ndim = length(datadim);
  if(ndim > 7L) {
    stop(sprintf("Length of datadim must be <= 7, but is %d. Not supported by NIFTI format, please reshape.\n", ndim));
  }
  dim_field[1] = ndim;
  dim_field[2:(2+ndim-1L)] = datadim;
  return(dim_field);
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
#' @return the string in the target encoding.
#'
#' @keywords internal
read.fixed.char.binary <- function(filehandle, n, to = "UTF-8") {
  txt = readBin(filehandle, "raw", n);
  return(iconv(rawToChar(txt[txt != as.raw(0)]), to = to));
}
