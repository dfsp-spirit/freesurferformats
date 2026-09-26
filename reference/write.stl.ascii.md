# Write the sections of a triangular mesh in ASCII STL format.

Writes the 'solid' block of the ASCII variant of the STL format, with 7
lines per face ('facet normal', 'outer loop', 3 'vertex' lines,
'endloop', 'endfacet'). This is the layout that
[`read.fs.surface.stl.ascii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.stl.ascii.md)
and other STL readers expect.

## Usage

``` r
write.stl.ascii(con, vertex_coords, faces, face_normals, solid_name = "mesh")
```

## Arguments

- con:

  a connection opened in binary write mode, the file is written as text
  through it.

- vertex_coords:

  n x 3 matrix of doubles, the vertex coordinates.

- faces:

  n x 3 matrix of integers, the vertex indices of the triangles.

- face_normals:

  n x 3 matrix of doubles, the normals of the faces, see
  [`mesh.face.normals`](https://dfsp-spirit.github.io/freesurferformats/reference/mesh.face.normals.md).

- solid_name:

  character string, the name of the mesh.

## Value

`NULL`, invisibly. The data are written to `con`.
