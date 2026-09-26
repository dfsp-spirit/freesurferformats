# Read DTI tracking data from file in MRtrix 'TCK' format.

Reads streamlines from a TCK file, optionally gzip-compressed.

## Usage

``` r
read.dti.tck(
  filepath,
  max_tracks = Inf,
  skip_tracks = 0L,
  bbox = NULL,
  chunk_values = 4e+06
)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

- max_tracks:

  numeric, the maximum number of tracks to read. Use `Inf` (the default)
  to read all tracks. This allows reading a subset of a very large
  tractogram without holding all of it in memory.

- skip_tracks:

  integer, the number of tracks to skip before reading any. Skipped
  tracks are never held in memory.

- bbox:

  numeric vector of length 6 or NULL. If given, only tracks that have at
  least one point inside the box are read, the box is given as
  `c(xmin, xmax, ymin, ymax, zmin, zmax)` in the coordinate system of
  the stored data. This has no effect for TSF files, which store no
  coordinates. The number of matching tracks cannot be known in advance,
  so for very large files this should be combined with a `max_tracks`
  value that is known to be safe.

- chunk_values:

  integer, the number of payload values that are read per chunk. This is
  an advanced tuning parameter that does not change the result, only the
  peak memory usage (at most one chunk is buffered at a time) and the
  granularity of the file reads. The default of 4e6 values corresponds
  to about 32 MB of doubles. Lower it on a machine with very little free
  memory.

## Value

named list with entries 'header' and 'tracks'. The tracks are stored in
an
[`is.fs.tracts`](https://dfsp-spirit.github.io/freesurferformats/reference/is.fs.tracts.md)
instance: all coordinates are kept in a single matrix and `tracks[[i]]`
returns the n x 3 coordinate matrix of the i-th track. Use
`fs.tracts.lengths(tck$tracks)` to get the number of points of each
track, and `as.list(tck$tracks)` to convert to a plain list of matrices.

## Examples

``` r
if (FALSE) { # \dontrun{
tckf <- "~/simple.tck"
tck <- read.dti.tck(tckf)

# Read only the first 1000 streamlines of a huge tractogram:
first_tracks <- read.dti.tck(tckf, max_tracks = 1000)

# Read the streamlines passing through a region:
region <- read.dti.tck(tckf, bbox = c(-10, 10, -20, 20, 0, 30))
} # }
```
