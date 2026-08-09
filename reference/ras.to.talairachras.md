# Compute MNI talairach coordinates from RAS coords.

Compute MNI talairach coordinates from RAS coords.

## Usage

``` r
ras.to.talairachras(ras_coords, talairach, invert_transform = FALSE)
```

## Arguments

- ras_coords:

  nx3 numerical vector, the input surface RAS coordinates. Could be the
  vertex coordinates of an 'fs.surface' instance, or the RAS coords from
  a surface label.

- talairach:

  the 4x4 numerical talairach matrix, or a character string which will
  be interpreted as the path to an xfm file containing the matrix
  (typically `$SUBJECTS_DIR/$subject/mri/transforms/talairach.xfm`).

- invert_transform:

  logical, whether to invert the transform. Do not use this, call
  `link{talairachras.to.ras}` instead.

## Value

the Talairach RAS coordinates for the given RAS coordinates

## Note

You can use this to compute the Talairach coordinate of a voxel, based
on its RAS coordinate.
