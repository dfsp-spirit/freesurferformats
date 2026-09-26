# Write the 348 byte ANALYZE 7.5 header to a connection.

Write the 348 byte ANALYZE 7.5 header to a connection.

## Usage

``` r
write.analyze.header.internal(fh, analyzeheader)
```

## Arguments

- fh:

  connection to write to.

- analyzeheader:

  named list, a valid ANALYZE 7.5 header.

## Value

invisible `NULL`, called for the side effect of writing to the
connection.
