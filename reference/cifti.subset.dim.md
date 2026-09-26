# Subset one dimension of an array.

Subset one dimension of an array.

## Usage

``` r
cifti.subset.dim(data, dim, selection)
```

## Arguments

- data:

  the array to subset.

- dim:

  integer, the dimension to subset.

- selection:

  integer vector or `NULL`, the indices to keep.

## Value

the subset, with the dimensions of `data` preserved.
