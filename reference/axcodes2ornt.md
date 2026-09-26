# Convert axis codes to an orientation array.

Convert axis codes to an orientation array.

## Usage

``` r
axcodes2ornt(axcodes)
```

## Arguments

- axcodes:

  character vector with one code per input axis, e.g. `c('L','P','S')`
  for a file whose first voxel axis points left.

## Value

numeric matrix with 3 rows and 2 columns, see
[`io.orientation`](https://dfsp-spirit.github.io/freesurferformats/reference/io.orientation.md).
