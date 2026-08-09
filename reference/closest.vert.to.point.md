# Find vertex index closest to given query coordinate using Euclidean distance.

Find vertex index closest to given query coordinate using Euclidean
distance.

## Usage

``` r
closest.vert.to.point(surface, point_coords)
```

## Arguments

- surface:

  an fs.surface instance or a nx3 numerical matrix representing mesh
  points.

- point_coords:

  nx3 matrix of query coords. If a vector, will be transformed `byrow`
  to such a matrix.

## Value

named list with entries: 'vertex_id' integer vector, the index of the
closest vertex, and 'dist': double vector, the Euclidean distance to
that vertex.

## See also

Other Euclidean distance util functions:
[`vertex.euclid.dist()`](https://dfsp-spirit.github.io/freesurferformats/reference/vertex.euclid.dist.md),
[`vertexdists.to.point()`](https://dfsp-spirit.github.io/freesurferformats/reference/vertexdists.to.point.md)
