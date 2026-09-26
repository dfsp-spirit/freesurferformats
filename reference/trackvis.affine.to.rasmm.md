# Compute the affine that maps TrackVis coordinates to RAS+ mm.

Computes the affine that turns the streamlines stored in a TRK file into
RAS+ mm coordinates in which a coordinate of (0,0,0) is the center of
the first voxel, which is the convention used by 'nibabel', DIPY and the
TRX format. The stored streamlines refer to the voxel corners and are in
"voxelmm" space, so this combines the stored matrix with the voxel
sizes, the half-voxel offset, and the orientation stored in the
`voxel_order` header field.

## Usage

``` r
trackvis.affine.to.rasmm(header)
```

## Arguments

- header:

  named list, the header of a TRK file as returned by
  [`read.dti.trk.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.header.md).

## Value

a 4x4 numeric matrix.

## Note

The TrackVis specification assumes that the stored streamlines are in mm
and that the stored matrix maps them to RAS, which means the matrix has
to be corrected whenever the voxels are not 1 mm^3. DSI Studio writes
files that do not need the half-voxel shift, which is what the
`shift_origin` parameter of
[`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md)
controls.
