# Refuse to read a CIFTI file as a volume or morphometry file.

The generic readers of this package dispatch on the file name, and a
CIFTI file is a NIFTI file as far as the name is concerned:
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md)
used to interpret a `.dscalar.nii` as a NIFTI morphometry file and
silently returned the raw matrix as a per-vertex vector (for the
official Conte69 `.dtseries`, a 121,902 element vector of a 60,951 x 2
matrix, i.e. plausible looking numbers in an order that means nothing),
and
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
failed with the unrelated 'This is not a one-file NIFTI format' message
of `oro.nifti`. This function detects the CIFTI files and stops with an
error that names the reader to use instead, see
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).

The check only costs a header read, and only for files whose name ends
with `.nii` or `.nii.gz`, since a CIFTI file always has such a name.

## Usage

``` r
cifti.stop.if.cifti(filepath)
```

## Arguments

- filepath:

  character string, the path to a CIFTI-2 file (usually one of
  `.dscalar.nii`, `.dtseries.nii`, `.dlabel.nii`, `.dconn.nii`,
  `.pscalar.nii`, `.ptseries.nii`, `.pconn.nii`, `.dpconn.nii` or
  `.pdconn.nii`). Note that this is not a NIFTI file, despite the `.nii`
  part. Gzipped CIFTI files are not supported, because the CIFTI-2
  format forbids compression.

## Value

`NULL`, invisibly. Stops if the file is a CIFTI file.
