# Read morphometry data from FreeSurfer NIFTI format files, determine NIFTI version automatically.

Read morphometry data from FreeSurfer NIFTI format files, determine
NIFTI version automatically.

## Usage

``` r
read.fs.morph.nii(filepath)
```

## Arguments

- filepath:

  path to a file in FreeSurfer NIFTI v1 or v2 format, potentially with
  the FreeSurfer hack for v1. See
  [`read.nifti1.data`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.data.md)
  and
  [`read.nifti2.data`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.data.md)
  for details.

## Value

numeric vector, the morphometry data
