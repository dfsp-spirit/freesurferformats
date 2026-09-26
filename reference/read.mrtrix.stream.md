# Read the data payload of an MRtrix TCK/TSF file.

Reads the payload sequentially in chunks and splits it into groups of
`values_per_point` finite values, which are the points of the individual
streamlines.

The end of the data is normally marked by a group of Inf values, but
that marker is not always present: the TSF test data shipped with this
package has none, and the MRtrix reference reader treats the plain end
of the file as a normal end of the streamline data as well. Its absence
is therefore not an error here, it is only reported as a warning when
the header states a larger number of streamlines than were found, i.e.,
when data really is missing.

## Usage

``` r
read.mrtrix.stream(
  con,
  offset,
  dsize,
  endian,
  values_per_point,
  max_groups = Inf,
  skip_groups = 0L,
  bbox = NULL,
  chunk_values = 4e+06,
  filepath = "",
  gzipped = FALSE,
  expected_groups = NA_real_
)
```

## Arguments

- con:

  a connection opened in binary read mode.

- offset:

  numeric, byte offset of the payload.

- dsize:

  integer, bytes per value (4 for Float32, 8 for Float64).

- endian:

  character string, 'little' or 'big'.

- values_per_point:

  integer, 3 for TCK (xyz triples) and 1 for TSF.

- max_groups:

  numeric, stop after this many groups have been selected. Use `Inf` to
  read everything.

- skip_groups:

  integer, the number of streamlines to skip before selecting any.
  Skipped streamlines are never held in memory.

- bbox:

  numeric vector of length 6 or NULL. If given, only streamlines that
  have at least one point inside the box are selected. The box is given
  as `c(xmin, xmax, ymin, ymax, zmin, zmax)` in the coordinate system of
  the stored data.

- chunk_values:

  integer, number of values to read per chunk.

- filepath:

  character string, the path of the file, used in messages only.

- gzipped:

  logical, whether the connection reads a gzip-compressed file. R cannot
  seek on gzip connections, so the header bytes are then skipped by
  reading and discarding them instead.

- expected_groups:

  numeric, the number of groups (streamlines) stated in the header, or
  NA if the header does not contain a usable count. Only used to warn
  about a possibly truncated file, never to limit reading.

## Value

named list with entries `data` (matrix with `values_per_point` columns
holding the concatenated groups), `lengths` (integer vector, number of
points per group) and `terminator_seen` (logical).
