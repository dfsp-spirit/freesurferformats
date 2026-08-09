# Check whether the dtype is suitable for the data.

This function provides an educated guess on whether the given dtype is
suitable for the data. It is usually called for the site effect of
printing warnings in case something seems off.

## Usage

``` r
check.dtype.for.data(mridata, mri_dtype_code)
```

## Arguments

- mridata:

  the data to check, a vector, matrix or array typically

- mri_dtype_code:

  integer, the MRI data type code. See
  [`translate.mri.dtype`](https://dfsp-spirit.github.io/freesurferformats/reference/translate.mri.dtype.md).

## Value

logical, whether the dtype could be suitable. This is only a guess, as
the checks are in no way complete.
