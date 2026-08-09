# Read NIFTI v1 header from file (which may contain the FreeSurfer hack).

Read NIFTI v1 header from file (which may contain the FreeSurfer hack).

## Usage

``` r
read.nifti1.header(filepath)
```

## Arguments

- filepath:

  path to a NIFTI v1 file (single file version), which can contain the
  FreeSurfer hack.

## Value

named list with NIFTI 1 header fields.

## Note

The FreeSurfer hack is a non-standard way to save long vectors (one
dimension greater than 32767 entries) in NIFTI v1 files. Files with this
hack are produced when converting MGH or MGZ files containing such long
vectors with the FreeSurfer 'mri_convert' tool.
