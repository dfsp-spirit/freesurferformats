# Determine whether an MGH volume is conformed.

In the FreeSurfer sense, *conformed* means that the volume is in coronal
primary slice direction, has dimensions 256x256x256 and a voxel size of
1 mm in all 3 directions. The slice direction can only be determined if
the header contains RAS information, if it does not, the volume is not
conformed.

## Usage

``` r
mgh.is.conformed(mgh_header, voxel_size_tolerance = 1e-04)
```

## Arguments

- mgh_header:

  Header of the mgh datastructure, as returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).

- voxel_size_tolerance:

  double, the tolerance to accept when comparing the voxel size to the
  required value of `1.0`. Defaults to `1e-4`. Leave this alone unless
  you know what you are doing.

## Value

logical, whether the volume is *conformed*.
