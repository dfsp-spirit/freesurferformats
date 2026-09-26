# Compute the matrix that maps FSL voxel coordinates of a volume to FSL world coordinates.

FSL does not use the world coordinates of the image header. Its tools
work in a space in which the voxel axes have unit length (the voxel
sizes are divided out) and in which the first voxel axis points to the
left, i.e. the transformation has a negative determinant, which is why
FSL calls its images 'radiological'. For a volume whose header already
uses that convention, the FSL space equals the header space; otherwise
the first axis is flipped, and the origin of that axis is moved to the
other end of the volume.

This function implements the mapping that both MRtrix3 (in
`transformconvert ... flirt_import`) and FreeSurfer (in
`lta_convert --infsl`) apply, and it was verified against both of them
on real data: the resulting transformation matrix is identical to the
one of these tools up to numerical precision, while the plain
composition of the header voxel-to-RAS matrices is not (it differs by
more than 100 mm on real data).

## Usage

``` r
fsl.scaled.voxel.matrix(geometry)
```

## Arguments

- geometry:

  named list, the volume geometry as returned by
  [`volume.geometry`](https://dfsp-spirit.github.io/freesurferformats/reference/volume.geometry.md).

## Value

4x4 numerical matrix, the transformation from (zero-based) voxel indices
to FSL world coordinates.
