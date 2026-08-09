# Check that a numeric vector or matrix contains only finite values.

Stops with an error if the input contains `NA`, `NaN`, or `Inf` values.

## Usage

``` r
check_all_finite(x, label = "data")
```

## Arguments

- x:

  a numeric vector or matrix.

- label:

  character string, a human-readable label for the data (used in the
  error message).

## Value

`TRUE` (invisibly) if all values are finite. Stops otherwise.
