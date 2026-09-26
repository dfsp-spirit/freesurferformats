# Write a fixed length character field of an ANALYZE 7.5 header.

Write a fixed length character field of an ANALYZE 7.5 header.

## Usage

``` r
write.analyze.char.field(fh, field_value, field_length, return_bytes = FALSE)
```

## Arguments

- fh:

  connection to write to.

- field_value:

  character string, the value to write.

- field_length:

  integer, the length of the field in bytes.

- return_bytes:

  logical, whether to return the raw bytes instead of writing them to
  the connection.

## Value

invisible `NULL` (or the raw vector of length `field_length` if
`return_bytes` is `TRUE`), called for the side effect of writing to the
connection.

## Note

The string is converted to bytes as ISO-8859-1 (latin-1), which is the
inverse of the conversion that
[`analyze.read.char.field`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.read.char.field.md)
applies when reading, so that the content of the field survives a read
and write cycle byte for byte. Characters that latin-1 cannot represent
are written as UTF-8 instead.
