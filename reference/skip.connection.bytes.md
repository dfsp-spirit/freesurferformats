# Skip over bytes of a connection, transparently handling gzip.

Skips forward from the current position of the connection. R cannot seek
on a gzfile connection (the underlying `gzseek()` fails with 'invalid or
incomplete compressed data' and only warns instead of failing loudly),
so compressed connections are skipped by reading and discarding the
bytes instead.

## Usage

``` r
skip.connection.bytes(con, num_bytes, gzipped, filepath = "")
```

## Arguments

- con:

  a connection opened in binary read mode.

- num_bytes:

  numeric, the number of bytes to skip, relative to the current position
  of the connection.

- gzipped:

  logical, whether the connection is a gzfile connection, as reported by
  `is.gzip.file`.

- filepath:

  character string, used in error messages only.

## Value

`TRUE`, invisibly.
