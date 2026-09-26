# Get all matrix dimensions of a CIFTI-2 file that hold brain models.

A single `MatrixIndicesMap` element can apply to both matrix dimensions
(this is the case for connectome files like `.dconn` and `.pconn`), so
the dimensions have to be counted, not the mapping elements.

## Usage

``` r
cifti.brainordinate.dims(cii)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

## Value

integer vector, the dimensions that contain brain models (usually one,
two for a file like `.dconn`).
