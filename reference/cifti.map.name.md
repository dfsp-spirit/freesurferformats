# Get the name of one named map of a CIFTI-2 file.

Get the name of one named map of a CIFTI-2 file.

## Usage

``` r
cifti.map.name(cii, dim, map = 1L)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- dim:

  integer, the matrix dimension that holds the named maps.

- map:

  integer, the number of the map (1-based).

## Value

character string, the map name, or `NULL` if the map is unnamed.
