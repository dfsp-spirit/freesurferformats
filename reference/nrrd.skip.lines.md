# Skip whole lines of a connection.

Used for the NRRD 'line skip' header field, which announces that the
data does not start directly behind the header but behind a number of
newline-terminated lines. Unlike `skip.connection.bytes`, this has to
work for connections that cannot be seeked in, so the bytes are read and
discarded.

## Usage

``` r
nrrd.skip.lines(con, num_lines, filepath = "")
```

## Arguments

- con:

  a connection opened in binary read mode.

- num_lines:

  integer, the number of lines to skip.

- filepath:

  character string, used in error messages only.

## Value

`TRUE`, invisibly.
