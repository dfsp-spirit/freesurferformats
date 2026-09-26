# Get the matrix dimension a brain structure lives in.

The counterpart of
[`cifti.other.dim`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.other.dim.md)
for a structure name: look up the dimension of the file that contains
brain models for the given structure. Needed by the user-facing readers,
which get a structure name from the user and must decide which dimension
of the data matrix it refers to.

## Usage

``` r
cifti.structure.data.dim(cii, structure)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- structure:

  character string, the canonical name of the brain structure.

## Value

integer, the matrix dimension.
