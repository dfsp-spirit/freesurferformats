# Transform a bounding box with an affine.

Returns the axis-aligned bounding box of the transformed box. For a
linear transformation the extremes of the image of a box are attained at
its corners, so transforming the 8 corners gives the exact bounding box
of the transformed data.

## Usage

``` r
# S3 method for class 'bbox'
transform(bbox, affine)
```

## Arguments

- bbox:

  numeric vector of length 6, `c(xmin, xmax, ymin, ymax, zmin, zmax)`.

- affine:

  4x4 numeric matrix.

## Value

numeric vector of length 6.
