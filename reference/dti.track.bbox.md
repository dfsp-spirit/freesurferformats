# Compute the bounding box of all tract coordinates in a file.

Streams through the tract file and returns the bounding box of all
coordinates, without keeping the coordinates in memory. This is useful
to determine the axis limits for plotting a large tractogram.

## Usage

``` r
dti.track.bbox(filepath, chunk_values = 4e+06)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

- chunk_values:

  integer, the number of payload values to read per chunk. Advanced
  tuning parameter, see
  [`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md).

## Value

numeric vector of length 6, `c(xmin, xmax, ymin, ymax, zmin, zmax)`, or
`NULL` if the file contains no coordinates. The coordinates are in the
coordinate system used by the file, see the note in
[`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md).

## Examples

``` r
if (FALSE) { # \dontrun{
bbox <- dti.track.bbox("brain.tck");
} # }
```
