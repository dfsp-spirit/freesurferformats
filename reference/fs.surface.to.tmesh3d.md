# Get an rgl tmesh3d instance from a brain surface mesh.

Convert `fs.surface` to `tmesh` without the `rgl` package.

## Usage

``` r
fs.surface.to.tmesh3d(surface)
```

## Arguments

- surface:

  an fs.surface instance, as returned
  [`freesurferformats::read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md).

## Value

a `tmesh3d` instance representing the surface, see `rgl::tmesh3d` for
details. It has classes `mesh3d` and `shape3d`.
