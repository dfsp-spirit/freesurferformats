# Convert scalar values for streamlines to a list of vectors.

Normalizes the several input forms accepted by
[`write.dti.tsf`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.tsf.md)
to a list of numeric vectors, one per streamline, and checks the lengths
for consistency.

## Usage

``` r
as.tsf.scalars(tracts, lengths = NULL)
```

## Arguments

- tracts:

  the input, see
  [`write.dti.tsf`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.tsf.md).

- lengths:

  integer vector or NULL, the number of values per streamline. Only used
  when `tracts` is a plain vector of values.

## Value

named list with entries `values` (list of numeric vectors) and `lengths`
(integer vector).
