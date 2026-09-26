# Write the 348 byte NIFTI v1 header (and the padding up to the data offset) to a connection.

Write the 348 byte NIFTI v1 header (and the padding up to the data
offset) to a connection.

## Usage

``` r
write.nifti1.header.internal(fh, niiheader)
```

## Arguments

- fh:

  connection to write to.

- niiheader:

  named list, a valid NIFTI v1 header.

## Value

invisible `NULL`, called for the side effect of writing to the
connection.
