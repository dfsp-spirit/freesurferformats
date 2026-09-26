# Compute the voxel-to-RAS matrix from the MATLAB sidecar file of an ANALYZE image.

Read the transformation matrix that SPM and FreeSurfer write into the
`.mat` file next to an ANALYZE image, and convert it to the convention
used by this package (0-based voxel indices, right-anterior-superior
world coordinates). See the geometry section of
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md)
for the details.

## Usage

``` r
analyze.mat.sidecar.to.vox2ras(matfile)
```

## Arguments

- matfile:

  character string, the path of the `.mat` file.

## Value

named list with the entries `vox2ras` (a 4x4 numeric matrix, or `NULL`
if the file could not be read or contains no usable matrix) and `reason`
(a character string describing the problem, or `NULL` on success).
