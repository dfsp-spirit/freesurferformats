# Compute RAS coords of center voxel.

Compute RAS coords of center voxel.

## Usage

``` r
mghheader.centervoxelRAS.from.firstvoxelRAS(header, first_voxel_RAS)
```

## Arguments

- header:

  Header of the mgh datastructure, as returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).
  The `c_r`, `c_a` and `c_s` values in the header do not matter of
  course, they are what is computed by this function.

- first_voxel_RAS:

  numerical vector of length 3, the RAS coordinate of the first voxel in
  the volume. The first voxel is the voxel with `CRS=1,1,1` in R, or
  `CRS=0,0,0` in C/FreeSurfer. This value is also known as *P0 RAS*.

## Value

numerical vector of length 3, the RAS coordinate of the center voxel.
Also known as *CRAS* or *center RAS*.
