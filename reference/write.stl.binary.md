# Write the sections of a triangular mesh in binary STL format.

Writes the binary variant of the STL format: an 80 byte header, a 4 byte
face count, and then 50 bytes per face (3 float32 values for the face
normal, 9 float32 values for the 3 vertex coordinates, and a zero uint16
attribute byte count). All values are little endian, as the format
requires. Note that the header must not start with the string 'solid',
which is how readers tell the ASCII and the binary variant apart.

## Usage

``` r
write.stl.binary(con, vertex_coords, faces, face_normals)
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

## Value

`NULL`, invisibly. The data are written to `con`.
