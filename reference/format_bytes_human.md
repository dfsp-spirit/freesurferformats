# Format a number of bytes for human consumption.

Used in the error messages of `validate_allocation_size`, so that sizes
below one gigabyte stay readable. A limit of a few hundred kilobytes
would otherwise be reported as '0.00 GB', which is useless when
debugging a failed read.

## Usage

``` r
format_bytes_human(num_bytes)
```

## Arguments

- num_bytes:

  single numeric value, the number of bytes.

## Value

character string, the size with a unit.
