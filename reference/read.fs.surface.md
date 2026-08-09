# Read file in FreeSurfer surface format or various mesh formats.

Read a brain surface mesh consisting of vertex and face data from a file
in FreeSurfer binary or ASCII surface format. For a subject (MRI image
pre-processed with FreeSurfer) named 'bert', an example file would be
'bert/surf/lh.white'.

## Usage

``` r
read.fs.surface(filepath, format = "auto")
```

## Arguments

- filepath:

  string. Full path to the input surface file. Note: gzipped files are
  supported and gz format is assumed if the filepath ends with ".gz".

- format:

  one of 'auto', 'asc', 'vtk', 'ply', 'gii', 'mz3', 'stl', 'byu', 'geo',
  'ico', 'tri', 'obj', 'off' or 'bin'. The format to assume. If set to
  'auto' (the default), binary format will be used unless the filepath
  ends with '.asc'.

## Value

named list. The list has the following named entries: "vertices": nx3
double matrix, where n is the number of vertices. Each row contains the
x,y,z coordinates of a single vertex. "faces": nx3 integer matrix. Each
row contains the vertex indices of the 3 vertices defining the face.
This datastructure is known as a is a *face index set*. WARNING: The
indices are returned starting with index 1 (as used in GNU R). Keep in
mind that you need to adjust the index (by substracting 1) to compare
with data from other software.

## See also

Other mesh functions:
[`faces.quad.to.tris()`](https://dfsp-spirit.github.io/freesurferformats/reference/faces.quad.to.tris.md),
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

## Examples

``` r
surface_file <- system.file("extdata", "lh.tinysurface",
  package = "freesurferformats", mustWork = TRUE
)
mesh <- read.fs.surface(surface_file)
cat(sprintf(
  "Read data for %d vertices and %d faces. \n",
  nrow(mesh$vertices), nrow(mesh$faces)
))
#> Read data for 5 vertices and 3 faces. 
```
