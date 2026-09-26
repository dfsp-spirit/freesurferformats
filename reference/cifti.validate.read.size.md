# Check the safety limit for reading a CIFTI-2 data matrix.

Check the requested allocation against the package safety limit (see
[`validate_allocation_size`](https://dfsp-spirit.github.io/freesurferformats/reference/validate_allocation_size.md))
and add a CIFTI specific hint to the error message if it is exceeded:
the data of a large file can be read in parts by selecting matrix
columns, which is not possible in the same way for other image formats.

## Usage

``` r
cifti.validate.read.size(dims, bytes_per_elem, filepath)
```

## Arguments

- dims:

  integer vector, the dimensions of the requested data matrix.

- bytes_per_elem:

  numeric, the number of bytes per data value.

- filepath:

  character string, the path of the file, used in the error message.

## Value

`NULL`, invisibly. Stops if the limit is exceeded.
