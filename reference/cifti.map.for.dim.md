# Get the CIFTI indices map for a matrix dimension.

Get the single `MatrixIndicesMap` (mapping) that describes the given
matrix dimension. A CIFTI-2 file can have several mappings, and one
mapping can apply to several dimensions (this is the case for connectome
files like `.dconn` and `.pconn`).

## Usage

``` r
cifti.map.for.dim(cii, dim)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- dim:

  integer, the matrix dimension to get the brain models for. CIFTI-2
  files have two dimensions, so this is usually 0 (Workbench calls it
  the ROW dimension) or 1 (the COLUMN dimension). See
  `read.cifti.header` for details on the dimensions.

## Value

the indices map, a named list, see
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).
