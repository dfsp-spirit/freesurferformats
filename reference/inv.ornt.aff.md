# Build the affine that undoes an orientation transform.

This is the equivalent of `nibabel.orientations.inv_ornt_aff()`: given
an orientation and the shape of the array it was applied to, it returns
the affine that maps coordinates in the transformed space back to
coordinates in the original space.

## Usage

``` r
inv.ornt.aff(ornt, shape)
```

## Arguments

- ornt:

  numeric matrix with 2 columns, the orientation.

- shape:

  numeric vector, the shape (dimensions) of the array.

## Value

a 4x4 numeric matrix.
