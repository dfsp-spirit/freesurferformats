# Check whether character string fields have less than or equal to expected length.

Check whether character string fields have less than or equal to
expected length.

## Usage

``` r
nifti.field.check.nchar.max(niiheader, fields, dlength)
```

## Arguments

- niiheader:

  named list, representing a NIFTI v1 or v2 header

- fields:

  vector of character string, the field names to check

- dlength:

  integer, the max length of all fields

## Value

logical, whether the checks were okay
