# Create an iterator over the tracts of a DTI tract file.

Reads a TRK, TCK or TSF file tract by tract, so that a tractogram of any
size can be processed with a constant amount of memory. This is the
low-level interface behind
[`dti.track.count`](https://dfsp-spirit.github.io/freesurferformats/reference/dti.track.count.md)
and friends, use it when neither reading a subset (`max_tracks`,
`skip_tracks`, `bbox` in
[`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md))
nor one of the aggregate functions fits your use case.

The returned object is an environment with the following entries:

- `next.track()`: returns the next tract (a n x 3 matrix for TCK, a
  numeric vector of per-point values for TSF, and a list with the
  entries `coords`, `num_points`, `scalars` and `properties` for TRK,
  just like `tracks[[i]]` does for the readers), or `NULL` when there
  are no more tracts.

- [`close()`](https://rdrr.io/r/base/connections.html): closes the
  underlying file connection. It is safe to call this more than once,
  and it is also called automatically when the iterator is garbage
  collected.

- `tracks.read`: the number of tracts returned so far.

- `filepath`, `format`: the file and its detected format.

## Usage

``` r
dti.track.iterator(
  filepath,
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

the iterator environment, see the description.

## Examples

``` r
if (FALSE) { # \dontrun{
itr <- dti.track.iterator("brain.tck");
total_points <- 0;
while (!is.null(track <- itr$next.track())) {
  total_points <- total_points + nrow(track);
}
itr$close();
} # }
```
