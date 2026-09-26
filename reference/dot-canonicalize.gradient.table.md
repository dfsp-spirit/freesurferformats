# Bring a gradient table into the canonical 'one row per volume' layout.

Gradient tables occur in two orientations in the wild: the *FSL* layout,
in which the first axis is the vector *component* (3 rows of N values
for b-vectors, a single row of N values for b-values), and the *volumes*
layout, in which each line is one *volume* (N rows of 3 values for
b-vectors, as used by HCP, and N rows of 4 values for the MRtrix
gradient table format). This helper maps both to one row per volume.

## Usage

``` r
.canonicalize.gradient.table(
  m,
  n_components,
  layout,
  what,
  square_is_ambiguous = FALSE
)
```

## Arguments

- m:

  numeric matrix, the table as read from the file.

- n_components:

  integer, the number of values per volume (3 for b-vectors, 4 for a
  gradient table, 1 for b-values).

- layout:

  character string, one of 'auto', 'components' or 'volumes'. How to
  interpret the input. With 'auto' (the default), the orientation is
  derived from the matrix dimensions.

- what:

  character string, a human-readable description of the data, used in
  the messages.

- square_is_ambiguous:

  logical, how to handle a table that has exactly `n_components` rows
  *and* columns. The b-vector *file* reader sets this to `TRUE`, because
  for a 3x3 b-vector file the orientation cannot be decided (and the two
  reference implementations disagree about it). For a matrix passed in
  memory, and for the MRtrix gradient table, whose format defines one
  row per volume, the default `FALSE` means that the rows are read as
  volumes.

## Value

a numeric matrix with one row per volume and `n_components` columns.

## Note

A square 3x3 *file* is ambiguous, and the two reference implementations
do not agree on it: MRtrix3 reads such a b-vectors file with the
components in the rows, while DIPY reads it with the volumes in the
rows. Like MRtrix3,
[`read.dti.bvec`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md)
treats the lines as components, and warns, because silently guessing
here would scramble the gradient directions. Matrices passed in memory
are read with one row per volume, which is the R convention, and no
warning is needed.
