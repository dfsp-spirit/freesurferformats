# Compute MGH primary slice direction

Compute MGH primary slice direction

## Usage

``` r
mghheader.primary.slice.direction(header)
```

## Arguments

- header:

  Header of the mgh datastructure, as returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).

## Value

character string, the slice direction. One of 'sagittal', 'coronal',
'axial' or 'unknown'.
