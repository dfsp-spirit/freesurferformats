# Describe a volume for an fs.transform.

Transformation files often record the geometry of the volumes they
relate, either completely (an LTA file stores the voxel dimensions, the
voxel sizes and the voxel-to-RAS direction vectors of both volumes) or
not at all. This helper turns that information into the `src` and `dst`
descriptors of an `fs.transform`.

The `frame` entry states which RAS coordinates the `vox2ras` matrix
produces: 'scanner' means that the matrix is used as given (this is what
an LTA file records), and 'tkreg' means FreeSurfer tkregister
coordinates, which are RAS coordinates with an identity rotation and the
origin at the center of the volume. A descriptor may be partial, e.g.
when only the target is known to be in tkregister space.

## Usage

``` r
volume.descriptor(
  path = NULL,
  dim = NULL,
  voxelsize = NULL,
  xras = NULL,
  yras = NULL,
  zras = NULL,
  cras = NULL,
  vox2ras = NULL,
  frame = "scanner",
  valid = NULL
)
```

## Arguments

- path:

  `NULL` or character string, the file name of the volume as recorded in
  the transform file.

- dim:

  `NULL` or integer vector of length 3, the volume dimensions (number of
  voxels along each axis).

- voxelsize:

  `NULL` or numerical vector of length 3, the size of a voxel in mm
  along each axis.

- xras:

  `NULL` or numerical vector of length 3, the RAS direction of the first
  voxel axis, scaled by the voxel size. This is the 'xras' entry of the
  volume info section of an LTA file.

- yras:

  `NULL` or numerical vector of length 3, the RAS direction of the
  second voxel axis, scaled by the voxel size.

- zras:

  `NULL` or numerical vector of length 3, the RAS direction of the third
  voxel axis, scaled by the voxel size.

- cras:

  `NULL` or numerical vector of length 3, the RAS coordinates of the
  **center** of the volume, i.e. of voxel index `dim/2`. This is not the
  RAS coordinate of the first voxel, and using it as the translation
  column of a voxel-to-RAS matrix is an error of half the field of view.
  The 'cras' entry of the volume info section of an LTA file follows
  this convention, as verified against `mri_info --cras`.

- vox2ras:

  `NULL` or 4x4 numerical matrix, the transformation from voxel indices
  to RAS coordinates. If not given but `dim`, `xras`, `yras`, `zras` and
  `cras` are, the matrix is computed from them in the convention that
  [`mghheader.vox2ras`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md)
  implements for MGH headers: zero-based voxel indices and the origin at
  `cras - Mdc_scaled * (dim/2)`. Verified against `mri_info --vox2ras`
  on a real volume.

- frame:

  character string, either 'scanner', 'tkreg' or 'fsl', see the
  description.

- valid:

  `NULL` or integer, the 'valid' flag of the volume info section of an
  LTA file, which states whether the recorded geometry could be used by
  FreeSurfer.

## Value

`NULL` if no information was given, a named list describing the volume
otherwise.
