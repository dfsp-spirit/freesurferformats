# Determine NIFTI file version information and whether file is a NIFTI file.

Determine NIFTI file version information and whether file is a NIFTI
file.

## Usage

``` r
nifti.file.version(filepath)
```

## Arguments

- filepath:

  path to a file in NIFTI v1 or v2 format.

## Value

integer, the NIFTI file version. One if `1` for NIFTI v1 files, `2` for
NIFTI v2 files, or `NULL` if the file is not a NIFTI file.
