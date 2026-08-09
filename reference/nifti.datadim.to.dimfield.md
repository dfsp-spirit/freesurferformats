# Compute NIFTI dim field for data dimension.

Compute NIFTI dim field for data dimension.

## Usage

``` r
nifti.datadim.to.dimfield(datadim)
```

## Arguments

- datadim:

  integer vector, the result of calling `dim` on your data. The length
  must be \<= 7.

## Value

NIFTI header `dim` field, an integer vector of length 8

## See also

Other NIFTI helper functions:
[`nifti.datadim.from.dimfield()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.datadim.from.dimfield.md)

## Examples

``` r
nifti.datadim.to.dimfield(c(256, 256, 256))
#> [1]   3 256 256 256   1   1   1   1
```
