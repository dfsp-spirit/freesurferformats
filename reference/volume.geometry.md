# Get the geometry of a volume.

Transformation matrices of the voxel-to-voxel kind can only be
interpreted relative to the volumes they relate, so the conversion
functions need the geometry of those volumes: their dimensions, voxel
sizes and the matrix that maps voxel indices to RAS coordinates.

The voxel indices are zero-based and the origin of the RAS space is the
one used by FreeSurfer for MGH headers and by NIfTI for the `sform`,
i.e. this is the same convention as
[`mghheader.vox2ras`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md).

## Usage

``` r
volume.geometry(volume)
```

## Arguments

- volume:

  an `fs.volume` instance (as returned by
  [`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
  or
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md)),
  a `nifti` instance (from the `oro.nifti` package), or a named list
  with the entries 'vox2ras_matrix', 'voldim' and 'voxelsize'.

## Value

named list with the entries 'vox2ras' (4x4 numerical matrix), 'dim'
(integer vector of length 3) and 'voxelsize' (numerical vector of length
3).
