# Scan exactly n values from source string.

Scan exactly n values from source string.

## Usage

``` r
scann(cstring, num = 1L, what = integer(), line_number = NULL)
```

## Arguments

- cstring:

  the input character string

- num:

  integer, the number of expected resulting items.

- line_number:

  optional integer, the line number (if the string represents a line
  from a text file). Will be printed in error message, if any.

## Value

vector of type integer or double
