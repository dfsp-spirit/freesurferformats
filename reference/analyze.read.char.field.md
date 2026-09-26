# Read a fixed length character field of an ANALYZE 7.5 header.

Read a fixed length character field of an ANALYZE 7.5 header.

## Usage

``` r
analyze.read.char.field(filehandle, n)
```

## Arguments

- filehandle:

  connection to read from.

- n:

  integer, the number of bytes of the field.

## Value

character string, the field content with the trailing zero bytes
removed. The bytes are interpreted as ISO-8859-1 (latin-1) and converted
to UTF-8, which cannot fail: the ANALYZE 7.5 fields `originator`,
`generated`, `patient_id` and friends contain whatever the software that
wrote the file put there, including bytes that are not valid UTF-8 (the
SPM software, for example, stores an image origin as 3 little endian
integers in the `originator` field). Decoding them as UTF-8 would fail
or return `NA` for a subset of the possible byte values.
