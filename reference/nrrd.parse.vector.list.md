# Parse a NRRD vector list value like '(1,0,0) (0,1,0) (0,0,1)'.

Used for the 'space directions' field. Any component may be the string
'none', which marks a non-space axis (e.g. the time axis of a 4D
volume).

## Usage

``` r
nrrd.parse.vector.list(value)
```

## Arguments

- value:

  character string, the field value.

## Value

numeric matrix with one row per vector, or NULL for 'none'. Rows of a
non-space axis contain NA.
