# Write mesh to file in FreeSurfer binary surface format

Write vertex coordinates and vertex indices defining faces to a file in
FreeSurfer binary surface format. For a subject (MRI image pre-processed
with FreeSurfer) named 'bert', an example file would be
'bert/surf/lh.white'. This function writes the triangle version of the
surface file format.

## Usage

``` r
write.fs.surface(filepath, vertex_coords, faces, format = "auto")
```

## Arguments

- filepath:

  string. Full path to the output curv file. If it ends with ".gz", the
  file is written in gzipped format. Note that this is not common, and
  that other software may not handle this transparently.

- vertex_coords:

  n x 3 matrix of doubles. Each row defined the x,y,z coords for a
  vertex.

- faces:

  n x 3 matrix of integers. Each row defined the 3 vertex indices that
  make up the face. WARNING: Vertex indices should be given in R-style,
  i.e., the index of the first vertex is 1. However, they will be
  written in FreeSurfer style, i.e., all indices will have 1
  substracted, so that the index of the first vertex will be zero.

- format:

  character string, the format to use. One of 'bin' for FreeSurfer
  binary surface format, 'asc' for FreeSurfer ASCII format, 'vtk' for
  VTK ASCII legacy format, 'ply' for Standford PLY format, 'off' for
  Object File Format, 'obj' for Wavefront object format, 'gii' for GIFTI
  format, 'mz3' for Surf-Ice MZ3 fomat, 'byu' for Brigham Young
  University (BYU) mesh format, or 'auto' to derive the format from the
  file extension given in parameter 'filepath'. With 'auto', a path
  ending in '.asc' is interpreted as 'asc', a path ending in '.vtk' as
  vtk, and so on for the other formats. Everything not matching any of
  these is interpreted as 'bin', i.e., FreeSurfer binary surface format.

## Value

character string, the format that was written. One of "tris" or "quads".
Currently only triangular meshes are supported, so always 'tris'.

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
[`write.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.asc.md),
[`write.fs.surface.byu()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.byu.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md),
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)

Other mesh export functions:
[`write.fs.surface.obj()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.obj.md),
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
write.fs.surface(tempfile(), mesh$vertices, mesh$faces)
} # }
```
