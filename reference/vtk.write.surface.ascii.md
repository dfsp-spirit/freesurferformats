# Write the sections of a triangular mesh in VTK ASCII format.

Write the sections of a triangular mesh in VTK ASCII format.

## Usage

``` r
vtk.write.surface.ascii(con, vertex_coords, faces, version)
```

## Arguments

- con:

  a connection opened for writing.

- vertex_coords:

  n x 3 matrix of doubles, the vertex coordinates.

- faces:

  n x 3 matrix of integers, the vertex indices of the faces, already
  converted to zero-based indices.

- version:

  character string, either '4.2' or '5.1'.

## Value

`NULL`, invisibly.
