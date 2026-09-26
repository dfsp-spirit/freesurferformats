# Check a matrix index selection.

Check a matrix index selection.

## Usage

``` r
cifti.check.index.selection(selection, max_index, what, filepath)
```

## Arguments

- selection:

  integer vector or `NULL`, the indices to check.

- max_index:

  integer, the size of the matrix dimension.

- what:

  character string, the name of the selection ('rows' or 'columns'),
  used in the error message.

- filepath:

  character string, the path of the file, used in the error message.

## Value

the selection as an integer vector, or `NULL`.
