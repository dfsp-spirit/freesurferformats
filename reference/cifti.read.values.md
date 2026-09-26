# Read raw values from the data section of a CIFTI-2 file.

Read raw values from the data section of a CIFTI-2 file.

## Usage

``` r
cifti.read.values(cii, num_values, skip_values = 0L, fh = NULL)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- num_values:

  integer, the number of values to read.

- skip_values:

  integer, the number of values to skip first.

- fh:

  optional connection to the file, positioned at the start of the data
  section if `skip_values` is 0. Will be opened (and closed) if left at
  `NULL`.

## Value

vector of values, see
[`read.nifti.values`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti.values.md).
