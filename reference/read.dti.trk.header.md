# Read the header of a TrackVis TRK file.

Reads and parses the 1000 byte header of a TRK file without touching the
track data. This is cheap even for huge tractograms and can be used to
inspect a file before deciding whether to read its tracks.

## Usage

``` r
read.dti.trk.header(filepath, shift_origin = TRUE)
```

## Arguments

- filepath:

  character string, path to a file in TRK format. Gzip-compressed files
  (typically named `.trk.gz`) are supported, the compression is detected
  from the file content, not from the file name.

- shift_origin:

  logical, whether to compute the corrected `vox2ras` matrix, see
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md).

## Value

named list, the header. It contains all fields of the TRK header as
described at `http://trackvis.org/docs/?subsect=fileformat`, plus
`vox2ras` (the matrix stored in the file) and, if `shift_origin` is
`TRUE`, `vox2ras_corrected`.

## Examples

``` r
if (FALSE) { # \dontrun{
hdr <- read.dti.trk.header("brain.trk");
hdr$n_count;      # number of tracks stated in the header
hdr$voxel_order;
} # }
```
