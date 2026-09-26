# Check that all indices are within a valid range.

Check that all indices are within a valid range.

## Usage

``` r
cifti.validate.index.range(indices, max_index, what, filepath = "")
```

## Arguments

- indices:

  integer vector, the indices to check.

- max_index:

  integer, the largest allowed index.

- what:

  character string, a description of what the indices are, used in the
  error message.

- filepath:

  character string, the file path, used in error messages.

## Value

`NULL`, invisibly. Stops with a descriptive error if an index is out of
range.
