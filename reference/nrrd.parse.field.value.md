# Parse a NRRD header field value.

Converts the string value of a header field into an R value: a number
for the integer and floating point fields, a vector for the list fields,
a vector or a matrix for the vector and matrix fields (the syntax is
`(1,2,3)`, several vectors separated by whitespace, and the value `none`
for a missing vector), and a character vector for the string list
fields.

## Usage

``` r
nrrd.parse.field.value(key, value)
```

## Arguments

- key:

  character string, the normalized field name, see `nrrd.field.key`.

- value:

  character string, the raw value from the header.

## Value

the parsed value.
