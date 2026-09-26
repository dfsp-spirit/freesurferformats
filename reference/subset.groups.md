# Extract selected groups from a concatenated matrix.

Extract selected groups from a concatenated matrix.

## Usage

``` r
# S3 method for class 'groups'
subset(points, lengths, sel)
```

## Arguments

- points:

  numeric matrix holding the concatenated rows of all groups.

- lengths:

  integer vector with the number of rows of each group.

- sel:

  integer vector, the indices of the groups to extract.

## Value

named list with entries `points` (matrix with the rows of the selected
groups) and `lengths` (their lengths).
