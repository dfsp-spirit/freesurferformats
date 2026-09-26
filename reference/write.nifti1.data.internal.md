# Write the voxel data of a NIFTI v1 file to a connection.

Write the voxel data of a NIFTI v1 file to a connection.

## Usage

``` r
write.nifti1.data.internal(fh, niidata, niiheader)
```

## Arguments

- fh:

  connection to write to.

- niidata:

  array of numeric data, or `NULL` to write no data at all.

- niiheader:

  named list, a valid NIFTI v1 header that describes the data.

## Value

the data that was written, after conversion to the data type stated in
the header, or `NULL` if `niidata` was `NULL`.
