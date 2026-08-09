# Compute MGH volume orientation string.

Compute MGH volume orientation string.

## Usage

``` r
mghheader.crs.orientation(header)
```

## Arguments

- header:

  Header of the mgh datastructure, as returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).

## Value

character string of length 3, one uppercase letter per axis. Each of the
three position is a letter from the alphabet: `LRISAP?`. The meaning is
`L` for left, `R` for right, `I` for inferior, `S` for superior, `P` for
posterior, `A` for anterior. If the direction cannot be computed, all
three characters are `?` for unknown. Of course, each axis (`L/R`,
`I/S`, `A/P`) is only represented once in the string.
