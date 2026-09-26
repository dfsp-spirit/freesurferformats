# Create an iterator over the tracts of an MRtrix TCK or TSF file.

Create an iterator over the tracts of an MRtrix TCK or TSF file.

## Usage

``` r
mrtrix.track.iterator(
  filepath,
  skip_tracks = 0L,
  bbox = NULL,
  chunk_values = 4e+06
)
```

## Arguments

- filepath:

  character string, path to the file.

- skip_tracks:

  integer, the number of tracts to skip.

- bbox:

  numeric vector of length 6 or NULL, a bounding box, see
  `read.dti.tck`.

- chunk_values:

  integer, the number of payload values to read per chunk. This only
  affects the peak memory usage and the I/O granularity of the iterator,
  and is rarely needed. For TRK files it is ignored, since those records
  are read one at a time.

## Value

the iterator environment.
