# Read the track records of a TRK file.

Reads track records from the current position of the connection until
the end of the file or until `max_tracks` records have been selected.
Coordinates and per-point scalars are stored interleaved in the file, so
each record is read with a single `readBin` call and split afterwards,
instead of reading one point at a time.

## Usage

``` r
read.trk.records(
  fh,
  endian,
  n_scalars,
  n_properties,
  max_tracks = Inf,
  expected_tracks = NA_real_,
  skip_tracks = 0L,
  bbox = NULL,
  transform = NULL,
  gzipped = FALSE,
  filepath = ""
)
```

## Arguments

- fh:

  a connection opened in binary read mode, positioned at the start of
  the first track record.

- endian:

  character string, 'little' or 'big'.

- n_scalars:

  integer, number of scalars per point.

- n_properties:

  integer, number of properties per track.

- max_tracks:

  numeric, maximum number of tracks to select. Use `Inf` to read until
  the end of the file.

- expected_tracks:

  numeric, the number of tracks stated in the header, or NA if the
  header does not contain a usable count. Used to report truncated
  files, never to limit reading.

- skip_tracks:

  integer, the number of tracks to skip before selecting any, see
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md).

- bbox:

  numeric vector of length 6 or NULL, a bounding box, see
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md).

- transform:

  4x4 numeric matrix or NULL. If given, it is applied to the coordinates
  of every track as it is read, before the bounding box filter is
  evaluated and before they are stored. This is used to filter in the
  target coordinate system without transforming the whole result twice.

- gzipped:

  logical, whether the connection reads a gzip-compressed file, see
  [`is.gzip.file`](https://dfsp-spirit.github.io/freesurferformats/reference/is.gzip.file.md).
  R cannot seek in such a connection, so skipped tracks are skipped by
  reading and discarding them.

- filepath:

  character string, the path of the file, used in error messages and for
  skipping in compressed files.

## Value

named list with entries `coords` (matrix with 3 columns and one row per
point), `lengths` (integer vector, points per track), `scalars` (matrix
or NULL) and `properties` (matrix or NULL).
