# Compute the 'datatype' and 'bitpix' fields used in the NIFTI1 header from an MGH/MGZ datatype code.

Compute the 'datatype' and 'bitpix' fields used in the NIFTI1 header
from an MGH/MGZ datatype code.

## Usage

``` r
nifti.dtypebitpix.info.from.mgh.dtype(mgh_dtype_code)
```

## Arguments

- mgh_dtype_code:

  integer, the MGH/MGZ datatype code (as returned by
  `translate.mri.dtype`).

## Value

named list with entries: `datatype` and `bitpix` containing the
translated data for the respective NIfTI-1 header fields.

## Note

This is useful to compute a NIFTI v1 header from an MGH header.
