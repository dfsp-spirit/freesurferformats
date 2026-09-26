# Get the other matrix dimension of a 2-dimensional CIFTI-2 matrix.

Get the other matrix dimension of a 2-dimensional CIFTI-2 matrix.

## Usage

``` r
cifti.other.dim(cii, dim)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- dim:

  integer, the matrix dimension to exclude.

## Value

integer, the other matrix dimension. Stops for files with more than two
matrix dimensions.
