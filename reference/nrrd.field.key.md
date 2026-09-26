# Parse a NRRD header field name into a lookup key.

NRRD field names are matched case-insensitively and some fields are
spelled in two ways ('line skip' and 'lineskip', 'data file' and
'datafile'), so a key is computed by lowercasing the name and dropping
everything that is not a letter or a digit.

## Usage

``` r
nrrd.field.key(name)
```

## Arguments

- name:

  character string, the field name as it occurs in the file.

## Value

character string, the normalized key.
