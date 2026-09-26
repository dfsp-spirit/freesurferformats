# Access a single tract of an fs.tracts instance.

For TCK data (see
[`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md)),
a tract is an n x 3 numeric matrix of coordinates. For TRK data (see
[`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md)),
a tract is a named list with the entries `coords` (n x 3 matrix),
`num_points` (integer), `scalars` (n x n_scalars matrix or NULL) and
`properties` (numeric vector or NULL).

This returns a copy of the requested tract, the data is stored in a
single matrix internally.

## Usage

``` r
# S3 method for class 'fs.tracts'
x[[i]]
```

## Arguments

- x:

  an `fs.tracts` instance.

- i:

  positive integer, the index of the tract to retrieve.

## Value

the tract, see the description.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck");
first_tract_coords <- tck$tracks[[1]];
} # }
```
