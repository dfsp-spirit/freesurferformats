# Split a string into fixed-length parts.

Split a string into fixed-length parts.

## Usage

``` r
linesplit.fixed(
  cline,
  length_per_part,
  num_parts_expected = NULL,
  error_tag = NULL
)
```

## Arguments

- cline:

  character string, the input line

- length_per_part:

  integer, number of characters per part

- num_parts_expected:

  integer, the number of parts. Leave at NULL if this is not known.

- error_tag:

  optional character string, how to identify the line in a parsing error
  message. Could be the line number, or whatever. Only relevant if
  'num_parts_expected' is not matched.
