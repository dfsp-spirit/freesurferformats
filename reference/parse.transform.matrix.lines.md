# Parse matrix from text file lines.

Parse matrix from text file lines.

## Usage

``` r
parse.transform.matrix.lines(file_lines, ignore_line_suffix = ";")
```

## Arguments

- file_lines:

  vector of 3 character strings, the matrix lines. The separator is
  assumed to be a single space.

- ignore_line_suffix:

  character string, a line suffix that will be stripped from the end of
  each line if it exists.

## Value

numerical 4x4 matrix, the parsed matrix
