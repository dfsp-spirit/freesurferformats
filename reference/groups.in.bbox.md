# Check which groups have at least one point inside a box.

Check which groups have at least one point inside a box.

## Usage

``` r
groups.in.bbox(points, lengths, bbox)
```

## Arguments

- points:

  numeric matrix with 3 columns, the concatenated points.

- lengths:

  integer vector with the number of points of each group.

- bbox:

  numeric vector of length 6: `c(xmin, xmax, ymin, ymax, zmin, zmax)`.

## Value

logical vector with one entry per group.
