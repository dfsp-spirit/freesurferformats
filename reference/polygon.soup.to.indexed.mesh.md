# Turn polygon soup into indexed mesh.

Some mesh file formats like STL do not store the faces as indices into a
vertex list ('indexed mesh'), but repeat all vertex coordinates for each
face ('polygon soup'). This function creates an indexed mesh from a
polysoup.

## Usage

``` r
polygon.soup.to.indexed.mesh(faces_vertex_coords, digits = 6)
```

## Arguments

- faces_vertex_coords:

  numerical matrix with *n* rows and 3 columns, the vertex coordinates
  of the faces. Each row contains the x,y,z coordinates of a single
  vertex, and three consecutive vertex rows form a triangular face.

- digits:

  the precision (number of digits after decimal separator) to use when
  to determine whether two x,y,z coords define the same vertex.

## Value

an indexed mesh, as an `fs.surface` instance (see
[`read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md)).
