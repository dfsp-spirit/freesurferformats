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
FreeSurfer for MGH files, `r_dtype`: the R data type.
