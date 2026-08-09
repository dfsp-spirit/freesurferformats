# Validate that a requested allocation does not exceed the safety limit.

Given dimension sizes and bytes per element, checks that the total
allocation size is safe. Catches negative/NA/Inf dimensions, integer
overflow (by converting to double), and enforces the max allocation
limit.

## Usage

``` r
validate_allocation_size(
  dims,
  bytes_per_elem,
  max_bytes = get_max_alloc_bytes()
)
```

## Arguments

- dims:

  numeric vector of dimension sizes (e.g., `c(256, 256, 256)`).

- bytes_per_elem:

  single numeric value, the number of bytes per element (e.g., `4` for
  float32).

- max_bytes:

  single numeric value, the maximum allowed allocation in bytes.
  Defaults to the result of
  [`get_max_alloc_bytes()`](https://dfsp-spirit.github.io/freesurferformats/reference/get_max_alloc_bytes.md).
  Pass `Inf` to disable the limit check (negative/NA/Inf dims are still
  rejected).

## Value

the total number of elements (as double), invisibly. The function stops
with an error if the allocation would be unsafe.
