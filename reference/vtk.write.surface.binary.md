# Write the sections of a triangular mesh in binary VTK format.

Write the sections of a triangular mesh in binary VTK format.

## Usage

``` r
vtk.write.surface.binary(con, vertex_coords, faces, version)
```

## Arguments

- con:

  a connection opened for binary writing.

- vertex_coords:

  n x 3 matrix of doubles, the vertex coordinates.

- faces:

  n x 3 matrix of integers, the vertex indices of the faces, already
  converted to zero-based indices.

- version:

  character string, either '4.2' or '5.1'.

## Value

`NULL`, invisibly.

## Note

Binary data in the VTK legacy format is always big endian, the format
has no way of expressing a different byte order.
