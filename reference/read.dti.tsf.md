# Read DTI tracking per-coord data from file in MRtrix 'TSF' format.

Reads per-vertex track scalars from a TSF file, optionally
gzip-compressed.

## Usage

``` r
read.dti.tsf(
  filepath,
  max_tracks = Inf,
  skip_tracks = 0L,
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

- chunk_values:

  integer, the number of payload values that are read per chunk. This is
  an advanced tuning parameter that does not change the result, only the
  peak memory usage (at most one chunk is buffered at a time) and the
  granularity of the file reads. The default of 4e6 values corresponds
  to about 32 MB of doubles. Lower it on a machine with very little free
  memory.

## Value

named list with entries 'header' and 'scalars'. The scalar data are
available in 3 representations: 'merged' (a vector of all values),
'lengths' (the number of values per track) and 'scalar_list' (a list of
vectors, one per track, which is expensive for large files and should be
avoided for whole-brain data in favour of 'merged' and 'lengths').

## Examples

``` r
if (FALSE) { # \dontrun{
tsff <- "~/simple.tsf"
tsf <- read.dti.tsf(tsff)
} # }
```
