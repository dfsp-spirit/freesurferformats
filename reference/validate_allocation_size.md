# Validate that a requested allocation does not exceed the safety limit.

Given dimension sizes and bytes per element, checks that the total
allocation size is safe. Catches negative/NA/Inf dimensions, integer
overflow (by converting to double), and enforces the max allocation
limit.

Note that `bytes_per_elem` must be the size of an element *in memory*,
which is not necessarily the size on disk:
[`readBin()`](https://rdrr.io/r/base/readBin.html) into a
[`numeric()`](https://rdrr.io/r/base/numeric.html) vector allocates 8
bytes per element even when the values are stored as 4 byte floats.

## Usage

``` r
validate_allocation_size(
  dims,
  bytes_per_elem,
  max_bytes = get_max_alloc_bytes(),
  label = NULL
)
```

## Arguments

- dims:

  numeric vector of dimension sizes (e.g., `c(256, 256, 256)`).

- bytes_per_elem:

  single numeric value, the number of bytes per element as stored in
  memory (e.g., `8` for a
  [`numeric()`](https://rdrr.io/r/base/numeric.html) vector).

- max_bytes:

  single numeric value, the maximum allowed allocation in bytes.
  Defaults to the result of
  [`get_max_alloc_bytes()`](https://dfsp-spirit.github.io/freesurferformats/reference/get_max_alloc_bytes.md).
  Pass `Inf` to disable the limit check (negative/NA/Inf dims are still
  rejected).

- label:

  character string or NULL, a human-readable description of what is
  being allocated. Included in the error message to help the user
  understand which part of a file the limit was hit on.

## Value

the total number of elements (as double), invisibly. The function stops
with an error if the allocation would be unsafe.
