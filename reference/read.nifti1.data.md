# Read raw NIFTI v1 data from file (which may contain the FreeSurfer hack).

Read raw NIFTI v1 data from file (which may contain the FreeSurfer
hack).

## Usage

``` r
read.nifti1.data(filepath, drop_empty_dims = TRUE, header = NULL)
```

## Arguments

- filepath:

  path to a NIFTI v1 file (single file version), which can contain the
  FreeSurfer hack.

- drop_empty_dims:

  logical, whether to drop empty dimensions in the loaded data array.

- header:

  optional nifti header obtained from
  [`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md).
  Will be loaded automatically if left at `NULL`.

## Value

the data in the NIFTI v1 file. Note that the NIFTI v1 header information
(scaling, units, etc.) is not applied in any way: the data are returned
raw, as read from the file. The information in the header is used to
read the data with the proper data type and size.

## Note

The FreeSurfer hack is a non-standard way to save long vectors (one
dimension greater than 32k entries) in NIFTI v1 files. Files with this
hack are produced when converting MGH or MGZ files containing such long
vectors with the FreeSurfer 'mri_convert' tool.
