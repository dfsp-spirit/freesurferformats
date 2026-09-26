# Create an iterator over the tracts of a TRK file.

Create an iterator over the tracts of a TRK file.

## Usage

``` r
trk.track.iterator(filepath, skip_tracks = 0L, bbox = NULL)
```

## Arguments

- filepath:

  character string, path to the file.

- skip_tracks:

  integer, the number of tracts to skip.

- bbox:

  numeric vector of length 6 or NULL, a bounding box, see
  `read.dti.tck`.

## Value

the iterator environment.
