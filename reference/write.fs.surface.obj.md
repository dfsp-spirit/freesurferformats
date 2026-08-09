# Write mesh to file in Wavefront object (.obj) format

The wavefront object format is a simply ASCII format for storing meshes.

## Usage

``` r
write.fs.surface.obj(filepath, vertex_coords, faces, vertex_colors = NULL)
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
  written in FreeSurfer style, i.e., all indices will have 1
  substracted, so that the index of the first vertex will be zero.

- vertex_colors:

  vector of vertex colors. Will be written after the x, y, z coords on
  vertex lines. WARNING: This is NOT part of the official OBJ standard,
  and may not work with other software and even break some parsers.

## Value

string the format that was written. One of "tris" or "quads". Currently
only triangular meshes are supported, so always 'tris'.

## Note

Do not confuse the Wavefront object file format (.obj) with the OFF
format (.off), they are not identical.

## See also

Other mesh export functions:
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.off()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.md),
[`write.fs.surface.off.ply2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.off.ply2.md),
[`write.fs.surface.ply()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply.md),
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
write.fs.surface.obj(tempfile(fileext = ".obj"), mesh$vertices, mesh$faces)
} # }
```
