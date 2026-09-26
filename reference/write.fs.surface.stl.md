# Write mesh to file in STL format (ASCII or binary).

The STL format (stereolithography, the format used for 3D printing)
stores a triangular mesh as a list of triangles, each with its vertex
coordinates repeated and with a normal vector, instead of storing a
vertex list and indices into it. Both the ASCII and the binary version
of the format are written by this function, the binary one being the
default since it is much smaller and it is what most software uses. The
resulting files can be read back with
[`read.fs.surface.stl`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.stl.md)
and are accepted by mesh viewers and slicers.

## Usage

``` r
write.fs.surface.stl(
  filepath,
  vertex_coords,
  faces,
  ascii = FALSE,
  solid_name = "mesh"
)
```

## Arguments

- filepath:

  character string, the path of the file to write.

- vertex_coords:

  n x 3 matrix of doubles, the vertex coordinates.

- faces:

  n x 3 matrix of integers, the vertex indices of the triangles. The STL
  format has no support for polygons with more than 3 vertices, so a
  quad mesh has to be converted first with
  [`faces.quad.to.tris`](https://dfsp-spirit.github.io/freesurferformats/reference/faces.quad.to.tris.md).

- ascii:

  logical, whether to write the ASCII version of the format. The default
  is the binary version, which is smaller by a factor of about 5 and
  which is what most mesh processing software writes. Use the ASCII
  version if the file has to be readable by humans or by software that
  supports only the ASCII variant.

- solid_name:

  character string, the name of the mesh. Only used in the ASCII
  version, where the format requires the name in the first and the last
  line of the file.

## Value

character string, the format that was written: 'tris'.

## Note

The normals of the triangles are computed from the vertex coordinates
(the STL format stores them, but no reader has to trust them). A
degenerate triangle, i.e., one whose vertices are collinear or
identical, has no normal, so a zero vector is written for it.

An indexed mesh is stored as a polygon soup in an STL file: every
triangle repeats the coordinates of its vertices. Reading such a file
back with
[`read.fs.surface.stl`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.stl.md)
merges the repeated vertices again (using the `digits` precision of that
function), so a round trip through an STL file preserves the geometry of
the mesh, but not the order or the count of the vertices in the vertex
list.

## See also

Other mesh export functions:
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.obj()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.obj.md),
[`write.fs.surface.off()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.md),
[`write.fs.surface.off.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.ply2.md),
[`write.fs.surface.ply()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply.md),
[`write.fs.surface.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply2.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Write a mesh as binary and as ASCII STL:
mesh <- read.fs.surface(system.file("extdata", "cube.stl", package = "freesurferformats"));
write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces);
write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces, ascii = TRUE);

# The file format is also chosen by the file name when using the generic
# writer:
write.fs.surface(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces);
} # }
```
