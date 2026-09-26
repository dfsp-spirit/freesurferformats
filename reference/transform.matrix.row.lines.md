# Format the rows of a transformation matrix for a text file.

The transformation file formats store the matrix as text, and the values
are written with enough significant digits so that reading the file back
gives the exact same double values. Fewer digits are not enough: 15
digits, as used by some other tools, lose up to a few units in the last
place of a double.

## Usage

``` r
# S3 method for class 'matrix.row.lines'
transform(matrix)
```

## Arguments

- matrix:

  numerical matrix, the matrix to format.

## Value

character vector with one entry per row of the matrix.
