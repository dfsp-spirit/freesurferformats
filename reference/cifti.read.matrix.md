# Read the data matrix of a CIFTI-2 file.

Read the data matrix of a CIFTI-2 file.

## Usage

``` r
cifti.read.matrix(cii, rows = NULL, columns = NULL)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- rows:

  integer vector or `NULL`, the indices of matrix dimension 0 to read.
  Indices are 1-based, like everywhere else in R, and they refer to the
  rows of the returned array, i.e. to the first CIFTI matrix dimension.
  Use `NULL` (the default) to read all of them. Note that this selection
  is applied after reading the whole matrix: use
  [`read.cifti.rows`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md)
  if the file is too large for that.

- columns:

  integer vector or `NULL`, the indices of matrix dimension 1 to read,
  i.e. the columns of the returned array. This is the matrix dimension
  that holds the brainordinates of a dense file (the grayordinates of a
  `.dscalar`, `.dtseries` or `.dlabel`), and it is also one of the two
  dimensions that hold them in a connectome file like a `.dconn`.
  Selecting columns avoids reading the rest of the file, which is the
  only practical way to read a small part of a huge file like a `.dconn`
  (9 to 38 GB): for a 2-dimensional matrix, one contiguous block of the
  file holds all values of one column. Note that `rows` does not have
  this property, selecting rows still reads all values of the file.

## Value

the data array, see
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
