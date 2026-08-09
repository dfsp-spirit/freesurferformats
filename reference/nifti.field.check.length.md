# Check whether vector has expected length.

Check whether vector has expected length.

## Usage

``` r
nifti.field.check.length(niiheader, fields, dlength)
```

## Arguments

- niiheader:

  named list, representing a NIFTI v1 or v2 header

- fields:

  vector of character string, the field names to check

- dlength:

  integer, the expected length of all fields

## Value

logical, whether the checks were okay
