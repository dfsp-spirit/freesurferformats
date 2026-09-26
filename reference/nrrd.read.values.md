# Read NRRD values from a connection or a file.

Reads the scalar values of a NRRD data section, dispatching on the
encoding. The connection has to be positioned at the start of the data.
For the compressed encodings the stream starts at the current position:
gzip data are read through a decompression filter (which also works when
the stream is preceded by junk, e.g. an ASCII header), bzip2 data have
to be decompressed in one piece, since R has no streaming bzip2
connection.

## Usage

``` r
nrrd.read.values(
  filepath,
  data_offset,
  num_values,
  info,
  type_info,
  num_values_exact = TRUE
)
```

## Arguments

- filepath:

  character string, path to the file that holds the data.

- data_offset:

  numeric or NA, the byte offset of the data in the file. NA means that
  the data start at the current position of a connection that cannot be
  seeked in.

- num_values:

  numeric, the number of values to read. For the LIST mode this is
  computed per file from the file size, see `nrrd.read.data`.

- info:

  named list, the parsed header, see `read.nrrd.header`.

- type_info:

  named list, the R data type information, see `nrrd.type.info`.

- num_values_exact:

  logical, whether `num_values` is exact (then a shorter result is an
  error) or an upper bound (then whatever is present is returned). The
  LIST mode uses it to accept the values of a single data file.

## Value

numeric or integer vector, the values.
