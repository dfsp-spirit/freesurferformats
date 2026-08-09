# S3 method to read a neuroimaging surface file.

Tries to read the file with all implemented surface format reader
methods. The file must exist. With the default settings, one can read
files in the following surface formats: 1) FreeSurfer binary surface
format (e.g., `surf/lh.white`). 2) FreeSurfer ASCII surface format
(e.g., `surf/lh.white,asc`). 3) GIFTI surface format, only if package
`gifti` is installed. See
[`gifti::read_gifti`](https://rdrr.io/pkg/gifti/man/readgii.html) for
details. Feel free to implement additional methods. Hint:keep in mind
that they should return one-based indices.

## Usage

``` r
read_nisurfacefile(filepath, methods = c("fsnative", "fsascii", "gifti"), ...)
```

## Arguments

- filepath:

  character string, the full path to the input surface file.

- methods:

  list of character strings, the formats to try. Each of these must have
  a function called `read_nisurface.<method>`, which must return an
  `fs.surface` instance on success.

- ...:

  parameters passed on to the individual methods

## Value

an instance of `fs.surface`, read from the file. See
[`read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md)
for details. If none of the reader methods succeed, an error is raised.

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
[`write.fs.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.md),
[`write.fs.surface.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.asc.md),
[`write.fs.surface.byu()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.byu.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md),
[`write.fs.surface.mz3()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.mz3.md),
[`write.fs.surface.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.vtk.md)

## Examples

``` r
surface_file <- system.file("extdata", "lh.tinysurface",
  package = "freesurferformats", mustWork = TRUE
)
mesh <- read_nisurface(surface_file)
mesh
#> Brain surface trimesh with 5 vertices and 3 faces.
#> -Surface coordinates: minimal values are (0.30, 0.30, 0.30), maximal values are (0.30, 0.30, 0.30).
```
