# Look up a field in a parsed NRRD header.

Look up a field in a parsed NRRD header.

## Usage

``` r
nrrd.field(fields, name, default = NULL)
```

## Arguments

- fields:

  named list, the fields of the header, as returned by
  `nrrd.parse.header`.

- name:

  character string, the field name to look up, e.g. 'space origin'.

- default:

  the value to return when the field is not present.

## Value

the value of the field, or `default` when the field is not present.
