# Read morphometry data from FreeSurfer NIFTI v1 format files.

Read morphometry data from FreeSurfer NIFTI v1 format files.

## Usage

``` r
read.fs.morph.ni1(filepath)
```

## Arguments

- filepath:

  path to a file in FreeSurfer NIFTI v1 format, potentially with the
  FreeSurfer hack. See
  [`read.nifti1.data`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.data.md)
  for details.

## Value

numeric vector, the morphometry data

## Note

This function uses our internal NIFTI reader that supports NIFTI v1
files with the FreeSurfer hack. This function assumes that the data in a
file is a 1D vector and flattens it accordingly. It is not suitable to
load NIFTI files with arbitrary dimensions.
