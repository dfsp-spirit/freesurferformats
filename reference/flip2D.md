# Flip a 2D matrix.

Flip a 2D matrix.

## Usage

``` r
flip2D(slice, how = "horizontally")
```

## Arguments

- slice:

  a 2D matrix

- how:

  character string, one of 'vertically' / 'v' or 'horizontally' / 'h'.
  Note that flipping *horizontally* means that the image will be
  mirrored along the central *vertical* axis. If `NULL` is passed, the
  passed value is returned unaltered.

## Value

2D matrix, the flipped matrix.
