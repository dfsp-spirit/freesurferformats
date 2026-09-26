# Split one chunk of MRtrix payload values into streamlines.

A streamline is a maximal run of finite rows. A run that is not followed
by a separator belongs to the next chunk and is returned as `pending`
instead. Runs that contain no points at all (two consecutive separators,
which MRtrix writes for empty streamlines) are dropped, which is what
the 'nibabel' reader does as well.

## Usage

``` r
# S3 method for class 'mrtrix.chunk'
split(mat, pending, finish_pending = FALSE)
```

## Arguments

- mat:

  numeric matrix with one column per point value, the rows of the chunk,
  already truncated at the end-of-file marker if the chunk contains one.

- pending:

  numeric matrix, the points of the streamline that is still being
  assembled from the previous chunk.

- finish_pending:

  logical, whether the chunk ended in the end-of-file marker. In that
  case the pending streamline is complete and is returned as a regular
  streamline, and it must not be dropped.

## Value

named list with entries `points` (matrix holding the completed
streamlines, concatenated along the rows), `lengths` (integer vector
with the number of points of each completed streamline) and `pending`
(matrix, the points of the trailing incomplete streamline).
