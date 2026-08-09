# Warn about common errors in combining data and datatype.

Warn about common errors in combining data and datatype.

## Usage

``` r
check_data_and_settings_consistency(
  index,
  data,
  datatype,
  intent,
  force = FALSE
)
```

## Arguments

- index:

  positive integer, the dataarray index to report. Makes it easier for
  the user to find the broken one.

- data:

  vector or matrix, the data to write to the GIFTI file. Checked against
  the datatype.

- datatype:

  NIFTI datatype string, the datatype to use when writing to the GIFTI
  file. Checked against the data.

- intent:

  NIFTI intent string, checked independently. In no way do we check
  whether it makes sense for the data.

## Note

The checks in here are in no way exhaustive.
