# Compute a transformation matrix from the fields of an ANALYZE 7.5 header.

Compute the voxel-to-RAS matrix for an ANALYZE 7.5 image, using the
conventions of SPM and of the original ANALYZE software. See the
"geometry" section of
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md)
for what this matrix is and is not.

## Usage

``` r
analyze.header.to.vox2ras(analyzeheader)
```

## Arguments

- analyzeheader:

  named list, an ANALYZE 7.5 header as returned by
  [`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).

## Value

a 4x4 numeric matrix, the voxel-to-RAS transformation matrix. The matrix
assumes that the first voxel axis points to the left (i.e. it contains
`-pix_dim[2]` in its first column), which is the convention of the
format. If the SPM origin is present in the header, the matrix maps the
origin to the world position that the SPM fields describe, otherwise the
center of the image is used as the origin, exactly as the reference
implementation of the format (nibabel) does it.
