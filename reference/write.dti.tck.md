# Write tracks to a file in MRtrix TCK format.

Writes streamlines in the TCK format described at
`https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html`.
The format stores a triplet of NaN values between streamlines and a
triplet of Inf values at the end. The output is read by MRtrix and by
[`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md).

## Usage

``` r
write.dti.tck(
  tracts,
  filepath,
  datatype = "Float32LE",
  gzip = NULL,
  header = list()
)
```

## Arguments

- tracts:

  the tracks to write, either an `fs.tracts` instance or a list of
  numeric matrices with 3 columns, see
  [`write.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.trk.md).

- filepath:

  character string, the path of the file to write.

- datatype:

  character string, one of 'Float32LE' (the default, and what MRtrix
  writes), 'Float32BE', 'Float64LE' or 'Float64BE'.

- gzip:

  logical or NULL, whether to gzip-compress the output. If `NULL` (the
  default), the file is compressed when the file name ends in '.gz'.

- header:

  named list of additional header entries to store in the file, e.g.,
  the header of the file the tracks were read from. The entries 'id',
  'datatype', 'count', 'file' and 'derived' are always computed by the
  readers and cannot be set.

## Value

the file path, invisibly.

## Note

Tracts without any point cannot be represented in the TCK format: they
are written as a bare delimiter, which every reader (including this
package and 'nibabel') drops again, so the file reads back with fewer
tracts than it was written from. Writing such a file raises a warning.
[`write.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.trk.md)
preserves empty tracts.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck", max_tracks = 1000);
write.dti.tck(tck$tracks, "first_1000.tck", header = tck$header);

# Round trip through a compressed file:
write.dti.tck(tck$tracks, "copy.tck.gz");
} # }
```
