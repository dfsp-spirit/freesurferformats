# Open a connection that transparently handles gzip compression.

Open a connection that transparently handles gzip compression.

## Usage

``` r
# S3 method for class 'maybe.gzip'
open(filepath, gzipped, mode = "rb")
```

## Arguments

- filepath:

  character string, path to the file.

- gzipped:

  logical, whether the file is gzip-compressed (see `is.gzip.file`).

- mode:

  character string, 'r' for text or 'rb' for binary.

## Value

a connection, call [`close()`](https://rdrr.io/r/base/connections.html)
on it when done.
