# Compute MGH orientation string and direction

Compute MGH orientation string and direction

## Usage

``` r
get.slice.orientation(Mdc)
```

## Arguments

- Mdc:

  numeric 3x3 matrix, typically from the `header$internal$Mdc` field as
  returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).

## Value

named list with entries: `orientation_string`: character string of
length 3, one uppercase letter per axis. `direction_name`: slice
direction, character string, one of 'sagittal', 'coronal', 'axial' or
'unknown'.
