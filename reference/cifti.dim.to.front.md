# Move one dimension of an array to the front.

Move one dimension of an array to the front.

## Usage

``` r
cifti.dim.to.front(data, dim)
```

## Arguments

- data:

  the array.

- dim:

  integer, the dimension to move, counted from 0 (CIFTI style).

## Value

the array with the requested dimension first, and all other dimensions
in their original order.
