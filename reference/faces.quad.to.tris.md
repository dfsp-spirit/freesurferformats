# Convert quadrangular faces or polygons to triangular ones.

Convert quadrangular faces or polygons to triangular ones.

## Usage

``` r
faces.quad.to.tris(quad_faces)
```

## Arguments

- quad_faces:

  nx4 integer matrix, the indices of the vertices making up the *n* quad
  faces.

## Value

*2nx3* integer matrix, the indices of the vertices making up the *2n*
tris faces.

## Note

This function does no fancy remeshing, it simply splits each quad into
two triangles.

## See also

Other mesh functions:
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
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)
