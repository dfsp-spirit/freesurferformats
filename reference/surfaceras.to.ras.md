# Translate surface RAS coordinates, as used in surface vertices and surface labels, to volume RAS.

Translate surface RAS coordinates, as used in surface vertices and
surface labels, to volume RAS.

## Usage

``` r
surfaceras.to.ras(
  header_cras,
  sras_coords,
  first_voxel_RAS = c(1, 1, 1),
  invert_transform = FALSE
)
```

## Arguments

- header_cras:

  an MGH header instance from which to extract the cras (center RAS), or
  the cras vector, i.e., a numerical vector of length 3

- sras_coords:

  nx3 numerical vector, the input surface RAS coordinates. Could be the
  vertex coordinates of an 'fs.surface' instance, or the RAS coords from
  a surface label. Use the orig surfaces.

- first_voxel_RAS:

  the RAS of the first voxel, see
  [`mghheader.centervoxelRAS.from.firstvoxelRAS`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.centervoxelRAS.from.firstvoxelRAS.md)
  for details. Ignored if 'header_cras' is a vector.

- invert_transform:

  logical, whether to invert the transform. Do not use this, call
  `link{ras.to.surfaceras}` instead.

## Value

the RAS coords for the input sras_coords

## Note

The RAS can be computed from Surface RAS by adding the center RAS
coordinates, i.e., it is nothing but a translation.
