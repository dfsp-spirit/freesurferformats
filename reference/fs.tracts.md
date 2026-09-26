# Create an fs.tracts instance from a compact tract representation.

Creates the compact container used by the DTI tract readers and writers.
Reading a track file with
[`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md),
[`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md)
or
[`read.dti.tsf`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tsf.md)
returns instances of this class, and this function is the way to build
one from your own data, e.g., to write a tractogram that was assembled
or edited in R with
[`write.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.tck.md)
or
[`write.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.trk.md).

## Usage

``` r
fs.tracts(coords, lengths, scalars = NULL, properties = NULL, kind = "tck")
```

## Arguments

- coords:

  numeric matrix with 3 columns, the concatenated coordinates of all
  tracts.

- lengths:

  integer vector, the number of points of each tract. Must sum up to
  `nrow(coords)`.

- scalars:

  numeric matrix or NULL. Per-point data, with one row per point (i.e.,
  `nrow(scalars) == nrow(coords)`).

- properties:

  numeric matrix or NULL. Per-tract data, with one row per tract.

- kind:

  character string, either 'tck' or 'trk'. Determines what `[[` returns
  for a single tract.

## Value

an `fs.tracts` instance.

## Examples

``` r
# Two tracts, the first with two points and the second with one.
coords <- matrix(c(0, 0, 0, 1, 1, 1, 5, 5, 5), ncol = 3, byrow = TRUE);
tracts <- fs.tracts(coords, lengths = c(2L, 1L));
length(tracts);
#> [1] 2
tracts[[1]];
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    1    1    1
```
