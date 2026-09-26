# Read raw voxel values of a NIFTI v1/v2 or ANALYZE file from a connection.

Read `num_values` voxel values of the data type described by the
`datatype` and `bitpix` fields of a header, in the endianness of the
file. This is the shared low level reading code of the NIFTI and ANALYZE
readers.

## Usage

``` r
read.nifti.values(fh, datatype, bitpix, num_values, endian)
```

## Arguments

- fh:

  connection to read from, positioned at the first value.

- datatype:

  integer, the `datatype` header field.

- bitpix:

  integer, the `bitpix` header field.

- num_values:

  integer, the number of values to read.

- endian:

  character string, the endianness of the file, 'little' or 'big'.

## Value

numeric or integer vector of length `num_values`, the raw values as they
are stored in the file.

## Note

The signedness of the data type is taken from
[`nifti.dtype.info`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.dtype.info.md):
an unsigned 8 bit value of 200 is returned as 200, not as -56. R's
`readBin` reads integers as signed by default, and it silently ignores a
`signed` argument for 4 byte integers, so the three unsigned types need
special care.
