# Rotate a 3D array in 90 degree steps.

Rotate a 3D array in 90 degree steps along an axis. This leads to an
array with different dimensions.

## Usage

``` r
rotate3D(volume, axis = 1L, degrees = 90L)
```

## Arguments

- volume:

  a 3D image volume

- axis:

  positive integer in range 1L..3L or an axis name, the axis to use.

- degrees:

  integer, must be a (positive or negative) multiple of 90L.

## Value

a 3D image volume, rotated around the axis. The dimensions may or may
not be different from the input image, depending on the rotation angle.

## See also

Other volume math:
[`flip3D()`](https://dfsp-spirit.github.io/freesurferformats/reference/flip3D.md)
