# Convert the NRRD type name of a volume into R data type information.

The NRRD spec names the data type of the values with a set of aliases
for the standard integer and floating point types ('uint8', 'unsigned
char', 'uchar', ...), see
`https://teem.sourceforge.net/nrrd/format.html`. The 64 bit integer
types have no R equivalent and are read as doubles, which is exact up to
2^53.

## Usage

``` r
nrrd.type.info(type_name, filepath = "")
```

## Arguments

- type_name:

  character string, the value of the 'type' header field.

- filepath:

  character string, the file the header was read from, used in error
  messages.

## Value

named list with entries `what` (the `readBin` type), `size` (bytes per
value), `signed` (logical, for integer types), `itemsize` (bytes per
value), `r_class` ('integer' or 'double'), `is_int64` (logical) and
`wide` (logical, whether the type has to be interpreted from its raw
bytes instead of being read by `readBin`).
