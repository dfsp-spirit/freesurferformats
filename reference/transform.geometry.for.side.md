# Determine the volume geometry of one side of a transformation.

Use the geometry recorded in the transformation itself if it is
available, and the volume passed by the caller otherwise.

## Usage

``` r
# S3 method for class 'geometry.for.side'
transform(tf, side, volume = NULL)
```

## Arguments

- tf:

  an `fs.transform` instance.

- side:

  character string, either 'src' or 'dst'.

- volume:

  `NULL` or a volume, see
  [`volume.geometry`](https://dfsp-spirit.github.io/freesurferformats/reference/volume.geometry.md).

## Value

`NULL` or a geometry list as returned by
[`volume.geometry`](https://dfsp-spirit.github.io/freesurferformats/reference/volume.geometry.md).
