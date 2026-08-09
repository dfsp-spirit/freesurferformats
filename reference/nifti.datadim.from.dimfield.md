# Compute data dimensions from the 'dim' field of the NIFTI (v1 or v2) header.

Compute data dimensions from the 'dim' field of the NIFTI (v1 or v2)
header.

## Usage

``` r
nifti.datadim.from.dimfield(dimfield)
```

## Arguments

- dimfield:

  integer vector of length 8, the `dim` field of a NIFTI v1 or v2
  header, as returned by
  [`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)
  or
  [`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md).

## Value

integer vector of length \<= 7. The lengths of the used data dimensions.
The 'dim' field always has length 8, and the first entry is the number
of actually used dimensions. The return value is constructed by
stripping the first field and returning the used fields.

## See also

Other NIFTI helper functions:
[`nifti.datadim.to.dimfield()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.datadim.to.dimfield.md)

## Examples

``` r
nifti.datadim.from.dimfield(c(3, 256, 256, 256, 1, 1, 1, 1))
#> [1] 256 256 256
```
