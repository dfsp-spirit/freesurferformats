# Format a possibly missing TRK header size for an error message.

Format a possibly missing TRK header size for an error message.

## Usage

``` r
.format.header.size(value)
```

## Arguments

- value:

  integer vector, the result of a `readBin` call, which is empty when
  the file ended before the requested position.

## Value

character string.
