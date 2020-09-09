

#' @title Read NIFTI v2 header from file.
#'
#' @param filepath path to a NIFTI v2 file.
#'
#' @return named list with NIFTI 2 header fields.
#'
#' @export
nifti2.header <- function(filepath) {
  niiheader = nifti2.header.internal(filepath, little_endian = TRUE);
  return(niiheader);
}


#' @title Read NIFTI v2 header from file.
#'
#' @inheritParams nifti2.header
#'
#' @param little_endian internal logical, leave this alone. Endianness will be figured out automatically, messing with this parameter only hurts.
#'
#' @return named list with NIFTI 2 header fields.
#'
#' @note See https://nifti.nimh.nih.gov/pub/dist/data/nifti2/ for test data. Thanks to Anderson Winkler for his post at https://brainder.org/2015/04/03/the-nifti-2-file-format/.
#'
#' @keywords internal
nifti2.header.internal <- function(filepath, little_endian = TRUE) {

  endian = 'little';
  if(! little_endian) {
    endian = 'big';
  }
  niiheader = list('endian' = endian);

  if (endsWith(filepath, '.gz')) {
    fh = gzfile(filepath, "rb");
  }
  else {
    fh = file(filepath, "rb");
  }
  on.exit({ close(fh) }, add=TRUE);

  niiheader$sizeof_hdr = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if(niiheader$sizeof_hdr != 540L) {
    if(niiheader$sizeof_hdr == 348L) {
      stop("File not in NIFTI 2 format: header size 348 looks like a NIFTI v1 file.");
    }
    if(little_endian == FALSE) { # if called with FALSE, the TRUE option was already checked.
      stop(sprintf("File not in NIFTI 2 format: invalid header size %d, expected 540.\n", niiheader$sizeof_hdr)); # nocov
    } else {
      return(nifti2.header.internal(filepath, little_endian = FALSE));
    }
  }

  niiheader$magic = read.fixed.char.binary(fh, 8L);
  niiheader$data_type = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$bitpix = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  niiheader$dim = readBin(fh, integer(), n = 8, size = 8, endian = endian);

  niiheader$intent_p1 = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$intent_p2 = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$intent_p3 = readBin(fh, numeric(), n = 1, size = 8, endian = endian);

  niiheader$pixdim = readBin(fh, numeric(), n = 8, size = 8, endian = endian);

  niiheader$vox_offset = readBin(fh, integer(), n = 1, size = 8, endian = endian);

  niiheader$scl_slope = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$scl_inter = readBin(fh, numeric(), n = 1, size = 8, endian = endian);

  niiheader$cal_max = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$cal_min = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$slice_duration = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$toffset = readBin(fh, numeric(), n = 1, size = 8, endian = endian);

  niiheader$slice_start = readBin(fh, integer(), n = 1, size = 8, endian = endian);
  niiheader$slice_end = readBin(fh, integer(), n = 1, size = 8, endian = endian);

  #niiheader$description = readBin(fh, character(), n = 1, endian = endian); # 80 bytes
  #niiheader$aux_file = readBin(fh, character(), n = 1, endian = endian); # 24 bytes
  niiheader$description = read.fixed.char.binary(fh, 80L);
  niiheader$aux_file = read.fixed.char.binary(fh, 24L);

  niiheader$qform_code = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  niiheader$sform_code = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  niiheader$quatern_b = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$quatern_c = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$quatern_d = readBin(fh, numeric(), n = 1, size = 8, endian = endian);

  niiheader$qoffset_x = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$qoffset_y = readBin(fh, numeric(), n = 1, size = 8, endian = endian);
  niiheader$qoffset_z = readBin(fh, numeric(), n = 1, size = 8, endian = endian);

  niiheader$srow_x = readBin(fh, numeric(), n = 4, size = 8, endian = endian);
  niiheader$srow_y = readBin(fh, numeric(), n = 4, size = 8, endian = endian);
  niiheader$srow_z = readBin(fh, numeric(), n = 4, size = 8, endian = endian);

  niiheader$slice_code = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  niiheader$xyzt_units = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  niiheader$intent_code = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  #niiheader$intent_name = readBin(fh, character(), n = 1, endian = endian); # 16 bytes
  niiheader$intent_name = read.fixed.char.binary(fh, 16L);
  niiheader$dim_info = readBin(fh, integer(), n = 1, size = 1, endian = endian);

  # Read the padding. May contain custom header extensions (used in CIFTI2), which we do not interprete.
  num_skip = 15L; # padding bytes to skip.
  discarded = readBin(fh, integer(), n = num_skip, size = 1L);
  discarded = NULL;

  return(niiheader);
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


