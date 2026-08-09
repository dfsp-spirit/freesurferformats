# Read Brainvoyager srf format (.srf) mesh as surface.

Read a mesh and associated data like color and normals from a binary
file in BrainVoyager SRF mesh format.

## Usage

``` r
read.fs.surface.bvsrf(filepath)
```

## Arguments

- filepath:

  string. Full path to the input surface file in SRF mesh format.

## Value

fs.surface instance

## References

The srf format spec is at
https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/344-users-guide-2-3-the-format-of-srf-files.

## See also

Other mesh functions:
[`faces.quad.to.tris()`](https://dfsp-spirit.github.io/freesurferformats/reference/faces.quad.to.tris.md),
[`read.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md),
[`read.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.asc.md),
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
