# Determine the orientation of an affine's input axes.

For each of the 3 input (voxel) axes, determines which output (world)
axis it corresponds to and whether it is flipped. This is the simplified
equivalent of `nibabel.orientations.io_orientation()`: the dominant
output axis of every input axis is used, with output axes removed from
consideration once they have been assigned. nibabel additionally applies
a polar decomposition first, which only matters for affines that contain
shear.

## Usage

``` r
io.orientation(affine)
```

## Arguments

- affine:

  4x4 numeric matrix.

## Value

numeric matrix with 3 rows and 2 columns, as returned by
`io.orientation`. Row i holds the 0-based output axis index and the
direction (+1 or -1) of input axis i.
