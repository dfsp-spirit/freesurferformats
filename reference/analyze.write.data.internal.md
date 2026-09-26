# Write the voxel data of an ANALYZE 7.5 image to a connection.

Write the voxel data of an ANALYZE 7.5 image to a connection.

## Usage

``` r
analyze.write.data.internal(fh, analyzedata, analyzeheader)
```

## Arguments

- fh:

  connection to write to.

- analyzedata:

  array of numeric data.

- analyzeheader:

  named list, a valid ANALYZE 7.5 header that describes the data.

## Value

the data that was written, after conversion to the data type stated in
the header.
