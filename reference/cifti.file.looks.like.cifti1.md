# Check whether a file looks like a CIFTI-1 file.

This is a heuristic used to produce a helpful error message when a
CIFTI-1 file is passed to the reader: CIFTI-1 files are NIFTI-1 files
(not NIFTI-2), and this function searches the first bytes of the file
for the string 'CIFTI'. It is not used for any other purpose.

## Usage

``` r
cifti.file.looks.like.cifti1(filepath)
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

logical, whether the file looks like a CIFTI-1 file.
