# Format numeric values for a gradient table text file.

Writes gradient values with enough significant digits to survive a round
trip, in a plain decimal or scientific notation that the FSL, MRtrix3,
DIPY and NumPy readers all accept. Note that `formatC(..., digits = 15)`
cannot be used here: with the default `width = NULL` it treats `digits`
as the field width and pads every value with spaces, which makes the
files hard to read.

## Usage

``` r
.format.gradient.values(x)
```

## Arguments

- x:

  numeric vector, the values to format.

## Value

character vector of the same length as `x`.
