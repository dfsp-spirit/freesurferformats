# Safe wrapper around readBin that verifies the number of elements read.

Calls [`readBin`](https://rdrr.io/r/base/readBin.html) and then checks
that the returned vector has the expected length. If the file ends
prematurely, `readBin` silently returns fewer elements — this wrapper
turns that into an explicit error.

## Usage

``` r
read_safe_bin(con, what, n, size, endian, ...)
```

## Arguments

- con:

  a connection object.

- what:

  character string or type, passed to `readBin`.

- n:

  integer, the number of elements to read.

- size:

  integer, the number of bytes per element.

- endian:

  character string, `"big"` or `"little"`.

- ...:

  additional arguments passed to `readBin`.

## Value

the vector of data read from the connection.
