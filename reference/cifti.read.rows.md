# Read the requested matrix rows of a CIFTI-2 file.

Workhorse of
[`read.cifti.rows`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md):
read some indices of matrix dimension 0 without reading the rest of the
rows. If a column selection is given, only those columns are read (they
are contiguous blocks of the file), otherwise the file is read once, in
chunks, keeping the requested rows.

## Usage

``` r
cifti.read.rows(cii, rows, columns = NULL, chunk_values = 4000000L)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- rows:

  integer vector, the indices of matrix dimension 0 to read. At least
  one index has to be given; the indices are 1-based, see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).

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

- chunk_values:

  integer, the number of data values that are read from the file per
  chunk. This does not change the result, only the peak memory usage and
  the I/O granularity, so it is rarely needed: the default of 4 millions
  values corresponds to about 16 MB. The chunk size is rounded up to a
  whole number of matrix columns, and the result is the same for every
  chunk size.

## Value

a numeric matrix with one row per requested row index.
