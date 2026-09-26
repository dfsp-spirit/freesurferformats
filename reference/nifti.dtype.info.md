# Compute NIFTI v1 data type info from datatype and bitpix header field.

Compute NIFTI v1 data type info from datatype and bitpix header field.

## Usage

``` r
nifti.dtype.info(datatype, bitpix)
```

## Arguments

- datatype:

  integer, the `datatype` NIFTI v1 header field

- bitpix:

  integer, the `bitpix` NIFTI v1 header field

## Value

named list with entries: `mri_dtype`: the MRI data type, as used by
FreeSurfer for MGH files, `r_dtype`: the R data type, `size`: the number
of bytes per value, `signed`: logical, whether the values are signed
(only meaningful for integer types, `NA` for floating point types) and
`is_float`: logical, whether the type is a floating point type.

## Note

The `signed` entry matters for reading the data: an unsigned 8 bit value
of 200 is read as -56 if it is read as a signed value, which is a silent
change of the data. See
[`read.nifti.values`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti.values.md).
