# Create an array like the input, with a different first dimension.

Create an array like the input, with a different first dimension.

## Usage

``` r
cifti.array.with.new.first.dim(data, first_dim_size)
```

## Arguments

- data:

  the array, its first dimension is the one that is replaced.

- first_dim_size:

  integer, the size of the new first dimension.

## Value

an array of `NA` of the data type of `data`, with the new first
dimension and all other dimensions of `data`.
