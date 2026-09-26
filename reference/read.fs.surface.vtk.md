# Read VTK legacy format mesh as surface.

Reads meshes from files in the VTK legacy format. Both the ASCII and the
binary encoding are supported, as are the cell array layouts written by
VTK 4.2 and older and by VTK 5.1 and newer, see the notes. See
<https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf> for the
format specification. Note that this function does **not** read
arbitrary VTK datasets, it supports only the geometry of POLYDATA
datasets (meshes and point clouds); attribute data such as normals,
texture coordinates or scalars is ignored. Only triangular meshes are
supported, files containing other polygons are rejected with an error.

## Usage

``` r
read.fs.surface.vtk(filepath)
```

## Arguments

- filepath:

  string. Full path to the input surface file in VTK format.

## Value

named list. The list has the following named entries: "vertices": nx3
double matrix, where n is the number of vertices. Each row contains the
x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each
row contains the vertex indices of the 3 vertices defining the face.
WARNING: The indices are returned starting with index 1 (as used in GNU
R). Keep in mind that you need to adjust the index (by substracting 1)
to compare with data from other software.

## Note

This is by far not a complete VTK format reader. Files that store
streamlines instead of a mesh (i.e., that contain a LINES section) are
read with
[`read.fs.tracts.vtk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.tracts.vtk.md).

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
[`read.mesh.brainvoyager()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.mesh.brainvoyager.md),
[`read_nisurface()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurface.md),
[`read_nisurfacefile()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurfacefile.md),
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.asc.md),
[`write.fs.surface.byu()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.byu.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md),
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)

## Examples

``` r
surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
mesh <- read.fs.surface.vtk(surface_file)
cat(sprintf("Read a mesh with %d vertices and %d faces.\n", nrow(mesh$vertices), nrow(mesh$faces)))
#> Read a mesh with 8 vertices and 12 faces.
```
