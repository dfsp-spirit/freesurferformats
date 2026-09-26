# Write the payload of an MRtrix streamlines file.

Writes the concatenated per-point values of all streamlines, separated
by a NaN value after every streamline, as the TCK and TSF formats
require. The data are written in chunks of streamlines, so that the
extra memory needed does not depend on the size of the tractogram. This
is shared by the TCK and the TSF writer, which differ only in the number
of values per point and in the terminator they append.

## Usage

``` r
write.mrtrix.streamlines(
  con,
  values,
  lengths,
  dsize,
  endian,
  terminator = NULL,
  chunk_tracks = 10000L
)
```

## Arguments

- con:

  a connection opened in binary write mode.

- values:

  numeric matrix, the concatenated per-point values of all streamlines,
  with one column per value (3 coordinates for the TCK format, one
  scalar for the TSF format).

- lengths:

  integer vector, the number of points of each streamline.

- dsize:

  integer, the number of bytes per value (4 or 8).

- endian:

  character string, 'little' or 'big'.

- terminator:

  numeric vector or NULL. If given, it is written after the last
  streamline (the TCK format appends a vector of Inf values, the TSF
  format has no terminator, so the NaN delimiter of the last streamline
  already ends the file).

- chunk_tracks:

  integer, the number of streamlines that are converted and written at
  once.

## Value

the number of streamlines written, invisibly.
