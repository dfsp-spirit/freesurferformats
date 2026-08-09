# Determine whether a NIFTI file uses the FreeSurfer hack.

Determine whether a NIFTI file uses the FreeSurfer hack.

## Usage

``` r
nifti.file.uses.fshack(filepath)
```

## Arguments

- filepath:

  path to a NIFTI v1 file (single file version), which can contain the
  FreeSurfer hack.

## Value

logical, whether the file header contains the FreeSurfer format hack.
See
[`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md)
for details. This function detects NIFTI v2 files, but as they cannot
contain the hack, it will always return `FALSE` for them.

## Note

Applying this function to files which are not in NIFTI format will
result in an error. See
[`nifti.file.version`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.file.version.md)
to determine whether a file is a NIFTI file.
