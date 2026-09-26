# Check whether a file is gzip-compressed, based on its magic bytes.

Uses the gzip magic number (0x1f 0x8b) rather than the file extension,
since files are regularly renamed or stripped of their extension.
Reading a gzip-compressed payload from a plain
[`file()`](https://rdrr.io/r/base/connections.html) connection silently
produces garbage.

## Usage

``` r
is.gzip.file(filepath)
```

## Arguments

- filepath:

  character string, path to the file to check.

## Value

logical, TRUE if the file starts with the gzip magic number.
