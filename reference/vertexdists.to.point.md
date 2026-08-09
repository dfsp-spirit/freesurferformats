# Compute Euclidean distance from all mesh vertices to given point.

Compute Euclidean distance from all mesh vertices to given point.

## Usage

``` r
vertexdists.to.point(surface, point_coords)
```

## Arguments

- surface:

  an fs.surface instance

- point_coords:

  double vector of length 3, the xyz coords of a single point.

## Value

double vector of distances

## See also

Other Euclidean distance util functions:
[`closest.vert.to.point()`](https://dfsp-spirit.github.io/freesurferformats/reference/closest.vert.to.point.md),
[`vertex.euclid.dist()`](https://dfsp-spirit.github.io/freesurferformats/reference/vertex.euclid.dist.md)
