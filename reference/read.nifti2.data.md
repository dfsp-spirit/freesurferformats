# Read raw data from NIFTI v2 file.

Read raw data from NIFTI v2 file.

## Usage

``` r
read.nifti2.data(filepath, header = NULL, drop_empty_dims = TRUE)
```

## Arguments

- filepath:

  path to a NIFTI v2 file.

- header:

  optional nifti v2 header obtained from
  [`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md).
  Will be loaded automatically if left at `NULL`.

- drop_empty_dims:

  logical, whether to drop empty dimensions in the loaded data array.

## Value

the data in the NIFTI v2 file. Note that the NIFTI v2 header information
(scaling, units, etc.) is not applied in any way: the data are returned
raw, as read from the file. The information in the header is used to
read the data with the proper data type and size.
