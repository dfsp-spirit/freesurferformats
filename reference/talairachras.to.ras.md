# Compute MNI talairach coordinates from RAS coords.

Compute MNI talairach coordinates from RAS coords.

## Usage

``` r
talairachras.to.ras(tal_ras_coords, talairach)
```

## Arguments

- tal_ras_coords:

  coordinate matrix in Talairach RAS space

- talairach:

  the 4x4 numerical talairach matrix, or a character string which will
  be interpreted as the path to an xfm file containing the matrix
  (typically `$SUBJECTS_DIR/$subject/mri/transforms/talairach.xfm`).

## Value

the Talairach RAS coordinates for the given RAS coordinates. They are
based on a linear transform.

## Note

You can use this to compute the Talairach coordinate of a voxel, based
on its RAS coordinate.

## References

see <https://en.wikipedia.org/wiki/Talairach_coordinates>
