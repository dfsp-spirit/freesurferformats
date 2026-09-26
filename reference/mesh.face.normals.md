# Compute the normals of the triangles of a mesh.

The normal of a triangle is the unit vector orthogonal to its plane, it
is computed as the normalized cross product of two of its edges. The STL
format stores one normal per triangle, and the value is computed from
the geometry instead of being taken from the data, since a mesh in index
representation does not store normals at all.

## Usage

``` r
mesh.face.normals(vertex_coords, faces)
```

## Arguments

- vertex_coords:

  n x 3 matrix of doubles, the vertex coordinates.

- faces:

  n x 3 matrix of integers, the vertex indices of the triangles.

## Value

n x 3 matrix of doubles, the normalized normal of every face. Rows of
degenerate triangles (whose 3 vertices lie on one line, which includes
triangles with repeated vertices) are zero vectors, since such triangles
have no plane and hence no normal.
