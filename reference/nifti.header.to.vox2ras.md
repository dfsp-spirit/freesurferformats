# Compute the voxel-to-RAS matrix from the geometry fields of a NIFTI v1 header.

The NIFTI v1 header can store up to two descriptions of the mapping from
voxel indices to world coordinates: the `sform` (a full affine matrix in
the `srow_x`, `srow_y` and `srow_z` fields) and the `qform` (a rotation,
given as a quaternion, plus a translation, given in the `quoffset`
fields). Each of them is only valid if the corresponding code field is
not zero, and the `sform` takes precedence over the `qform` if both are
present. This is the same rule that the NIFTI standard defines and that
other implementations (`nibabel`, `oro.nifti`) follow.

## Usage

``` r
nifti.header.to.vox2ras(niiheader)
```

## Arguments

- niiheader:

  named list, a NIFTI v1 header as returned by
  [`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md).

## Value

a 4x4 numeric matrix (the voxel-to-RAS transformation), or `NULL` if the
header contains neither an `sform` nor a `qform`. The matrix implements
the NIFTI convention that the rotation of the `qform` applies to the
*left* of the scaled voxel axes and that the third axis is flipped if
the `qfac` field (`pix_dim[1]`) is negative.

## Note

This is a header based re-implementation of the geometry computation of
[`read.fs.volume.nii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.nii.md),
which reads an `oro.nifti` instance. The two are compared against each
other in the unit tests, and against `nibabel` in
`dev_tools/check_analyze_conversion.R`.
