# Read Stanford PLY format mesh as surface.

This reads meshes from text files in PLY format. Note that this does not
read arbitrary data from PLY files, i.e., PLY files can store data that
is not supported by this function.

## Usage

``` r
read.fs.surface.ply(filepath)
```

## Arguments

- filepath:

  string. Full path to the input surface file in Stanford Triangle (PLY)
  format.

## Value

named list. The list has the following named entries: "vertices": nx3
double matrix, where n is the number of vertices. Each row contains the
x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each
row contains the vertex indices of the 3 vertices defining the face.
WARNING: The indices are returned starting with index 1 (as used in GNU
R). Keep in mind that you need to adjust the index (by substracting 1)
to compare with data from other software.

## Note

This is by far not a complete PLY format reader. It can read PLY mesh
files which were written by
[`write.fs.surface.ply`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.ply.md)
and Blender. Vertex colors and Blender vertex normals are currently
ignored (but files with them are supported in the sense that the mesh
data will be read correctly).

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
[`read.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.vtk.md),
[`read.mesh.brainvoyager()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.mesh.brainvoyager.md),
[`read_nisurface()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurface.md),
[`read_nisurfacefile()`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurfacefile.md),
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.asc.md),
[`write.fs.surface.byu()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.byu.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md),
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)
