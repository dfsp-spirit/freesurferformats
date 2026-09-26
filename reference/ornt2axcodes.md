# Convert an orientation array to axis codes.

Convert an orientation array to axis codes.

## Usage

``` r
ornt2axcodes(ornt)
```

## Arguments

- ornt:

  numeric matrix with 2 columns, see
  [`io.orientation`](https://dfsp-spirit.github.io/freesurferformats/reference/io.orientation.md).

## Value

character vector with one code per input axis, drawn from
`c('L','R','P','A','I','S')`. Dropped axes become NA.
