# Write mesh to file in PLY format (.ply)

The PLY format is a versatile ASCII format for storing meshes. Also
known as Polygon File Format or Stanford Triangle Format.

## Usage

``` r
write.fs.surface.ply(filepath, vertex_coords, faces, vertex_colors = NULL)
```

## Arguments

- filepath:

  string. Full path to the output surface file, should end with '.vtk',
  but that is not enforced.

- vertex_coords:

  n x 3 matrix of doubles. Each row defined the x,y,z coords for a
  vertex.

- faces:

  m x 3 matrix of integers. Each row defined the 3 vertex indices that
  make up the face. WARNING: Vertex indices should be given in R-style,
  i.e., the index of the first vertex is 1. However, they will be
  written in FreeSurfer style, i.e., all indices will have 1
  substracted, so that the index of the first vertex will be zero.

- vertex_colors:

  optional, matrix of RGBA vertex colors, number of rows must be the
  same as for vertex_coords. Color values must be integers in range
  0-255. Alternatively, a vector of *n* RGB color strings can be passed.

## Value

string the format that was written. One of "tris" or "quads". Currently
only triangular meshes are supported, so always 'tris'.

## References

See http://paulbourke.net/dataformats/ply/ for the PLY format spec.

## See also

Other mesh export functions:
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.obj()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.obj.md),
[`write.fs.surface.off()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.md),
[`write.fs.surface.off.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.ply2.md),
[`write.fs.surface.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply2.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a surface from a file:
surface_file <- system.file("extdata", "lh.tinysurface",
  package = "freesurferformats", mustWork = TRUE
)
mesh <- read.fs.surface(surface_file)
# Now save it:
write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices, mesh$faces)
# save a version with RGBA vertex colors
vertex_colors <- matrix(rep(82L, 5 * 4), ncol = 4)
write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices,
  mesh$faces,
  vertex_colors = vertex_colors
)
} # }
```
