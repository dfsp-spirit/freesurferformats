# Constructor to init MGH header instance.

Constructor to init MGH header instance.

## Usage

``` r
mghheader(dims, mri_dtype_code)
```

## Arguments

- dims:

  integer vector of length 4, the header dimensions. Example:
  `c(256L, 256L, 256L, 1L)`.

- mri_dtype_code:

  integer, a valid MRI datatype. See
  [`translate.mri.dtype`](https://dfsp-spirit.github.io/freesurferformats/reference/translate.mri.dtype.md).

## Value

a named list representing the header
