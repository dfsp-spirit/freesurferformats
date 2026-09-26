# Assign rows of an array, reordered or repeated.

Assign rows of an array, reordered or repeated.

## Usage

``` r
cifti.assign.rows(target, to, from, data)
```

## Arguments

- target:

  array, the array to write to.

- to:

  integer vector, the row indices of `target` to write.

- from:

  integer vector, the row indices of `data` to read.

- data:

  array, the source array.

## Value

`target` with the requested rows assigned.
