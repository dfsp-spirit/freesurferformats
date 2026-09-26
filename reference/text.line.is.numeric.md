# Check whether a text line holds a fixed number of numerical values.

Check whether a text line holds a fixed number of numerical values.

## Usage

``` r
# S3 method for class 'line.is.numeric'
text(line, num)
```

## Arguments

- line:

  character string, the line to check.

- num:

  integer, the number of numerical values expected in the line.

## Value

logical, whether the line contains exactly `num` numerical values and
nothing else.
