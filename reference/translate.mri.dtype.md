# Translate between code and name of MRI data types.

Translate between code and name of MRI data types.

## Usage

``` r
translate.mri.dtype(dtype)
```

## Arguments

- dtype:

  character string (one of c('MRI_FLOAT') or integer, one of c(0L, 1L,
  3L, 4L). Numeric values will be converted to integer.

## Value

if `dtype` is a character string, the respective integer code. If it is
numeric, the respective character string.
