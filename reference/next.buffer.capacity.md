# Compute the next capacity for a growing result buffer.

Validates that the data actually needed fits into the configured
allocation limit, then returns the new buffer capacity to allocate. The
capacity grows geometrically to keep the number of reallocations
logarithmic, but it is capped at the limit: an overshoot of the growth
must not make a read fail that would have fit into memory, and it must
not exceed the limit either.

## Usage

``` r
next.buffer.capacity(needed, capacity, bytes_per_elem = 8, label = NULL)
```

## Arguments

- needed:

  numeric, the number of rows that the buffer must hold.

- capacity:

  numeric, the current capacity.

- bytes_per_elem:

  numeric, bytes per row (for integer vectors this is the size of a
  single element).

- label:

  character string or NULL, description used in error messages.

## Value

numeric, the new capacity.
