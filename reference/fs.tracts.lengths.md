# Get the number of points of each tract.

Returns one integer per tract, the number of points it consists of. The
coordinates of tract `i` are the rows
`(cumsum(c(1, lengths))[i]):(cumsum(lengths)[i])` of
[`fs.tracts.coords()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.tracts.coords.md).

## Usage

``` r
fs.tracts.lengths(tracts)
```

## Arguments

- tracts:

  an `fs.tracts` instance, as returned in the `tracks` entry of
  `read.dti.tck` or `read.dti.trk`.

## Value

integer vector with one entry per tract.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck");
lengths <- fs.tracts.lengths(tck$tracks);
mean(lengths);
} # }
```
