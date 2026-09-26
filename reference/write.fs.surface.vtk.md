# Write mesh to file in VTK legacy format

The VTK legacy format is the plain text/binary format that is supported
by all versions of the VTK library; the XML based VTK format (.vtp) is
not the same thing. Two variants of the file layout exist and are both
still written by software in use today: the old layout introduced in VTK
4.2, and the layout that VTK produces since version 5.1 (released 2015).
The parameter 'version' selects which one to write.

## Usage

``` r
write.fs.surface.vtk(
  filepath,
  vertex_coords,
  faces,
  version = 4.2,
  binary = FALSE
)
```

## Arguments

- filepath:

  string. Full path to the output surface file, should end with '.vtk',
  but that is not enforced.

- vertex_coords:

  n x 3 matrix of doubles. Each row defined the x,y,z coords for a
  vertex.

- faces:

  n x 3 matrix of integers. Each row defined the 3 vertex indices that
  make up the face. WARNING: Vertex indices should be given in R-style,
  i.e., the index of the first vertex is 1. However, they will be
  written in VTK style, i.e., all indices will have 1 substracted, so
  that the index of the first vertex will be zero.

- version:

  double, the VTK version whose file layout to write. Either 4.2 (the
  default) or 5.1. Version 4.2 writes the cell array layout that every
  VTK version can read, version 5.1 writes the `OFFSETS`/`CONNECTIVITY`
  layout that VTK itself has been producing since 2015. Only change this
  if you know that the software you hand the file to requires the newer
  layout.

- binary:

  logical, whether to write the data in binary form instead of the ASCII
  text form. Binary files are much smaller and much faster to read and
  write, but they are not human readable. Defaults to FALSE.

## Value

string the format that was written. One of "tris" or "quads". Currently
only triangular meshes are supported, so always 'tris'.

## Note

Binary data in the VTK legacy format is always big endian, the format
has no way of expressing a different byte order. The vertex coordinates
are written as single precision (4 byte) floats in both encodings, which
is what VTK itself does.

## See also

Other mesh functions:
[`faces.quad.to.tris()`](https://dfsp-spirit.github.io/freesurferformats/reference/faces.quad.to.tris.md),
[`read.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md),
[`read.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.asc.md),
[`read.fs.surface.bvsrf()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.bvsrf.md),
[`read.fs.surface.geo()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.geo.md),
[`read.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.gii.md),
[`read.fs.surface.ico()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.ico.md),
[`read.fs.surface.obj()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.obj.md),
[`read.fs.surface.off()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.off.md),
[`read.fs.surface.ply()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.ply.md),
[`read.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.vtk.md),
[`read.mesh.brainvoyager()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.mesh.brainvoyager.md),
[`read_nisurface()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurface.md),
[`read_nisurfacefile()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurfacefile.md),
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.asc.md),
[`write.fs.surface.byu()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.byu.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md),
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md)

Other mesh export functions:
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.obj()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.obj.md),
[`write.fs.surface.off()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.md),
[`write.fs.surface.off.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.ply2.md),
[`write.fs.surface.ply()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply.md),
[`write.fs.surface.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply2.md),
[`write.fs.surface.stl()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.stl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a surface from a file:
surface_file <- system.file("extdata", "lh.tinysurface",
  package = "freesurferformats", mustWork = TRUE
)
mesh <- read.fs.surface(surface_file)
# Now save it:
write.fs.surface.vtk(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces)
# Or as a binary file using the layout of VTK 5.1:
write.fs.surface.vtk(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces,
  version = 5.1, binary = TRUE
)
} # }
```
