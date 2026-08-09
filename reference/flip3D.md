# Flip a 3D array along an axis.

Flip the slice of an 3D array horizontally or vertically along an axis.
This leads to an output array with identical dimensions.

## Usage

``` r
flip3D(volume, axis = 1L, how = "horizontally")
```

## Arguments

- volume:

  a 3D image volume

- axis:

  positive integer in range 1L..3L or an axis name, the axis to use.

- how:

  character string, one of 'horizontally' / 'h' or 'vertically' / 'v'.
  How to flip the 2D slices. Note that flipping *horizontally* means
  that the image will be mirrored along the central *vertical* axis.

## Value

a 3D image volume, flipped around the axis. The dimensions are identical
to the dimensions of the input image.

## See also

Other volume math:
[`rotate3D()`](https://dfsp-spirit.github.io/freesurferformats/reference/rotate3D.md)
