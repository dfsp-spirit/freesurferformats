# Compute the bounding box of a set of coordinates.

Compute the bounding box of a set of coordinates.

## Usage

``` r
coord.bbox(coords)
```

## Arguments

- coords:

  numeric matrix with 3 columns.

## Value

numeric vector of length 6, `c(xmin, xmax, ymin, ymax, zmin, zmax)`, or
NULL if there are no rows.
