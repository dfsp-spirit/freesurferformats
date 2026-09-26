# Get the dense data matrix of one structure for the user-facing readers.

Workhorse of
[`read.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.cifti.md)
and friends: like
[`cifti.structure.data`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md),
but requires the structure to have a surface part and returns the
per-vertex matrix directly. This keeps the user-facing functions working
for the structures they support today, and turns the unsupported ones
into a descriptive error.

## Usage

``` r
cifti.dense.structure.matrix(x, structure)
```

## Arguments

- x:

  an `fs.cifti.data` object, a file path or an `fs.cifti` object, see
  [`cifti.structure.data`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md).

- structure:

  character string, the canonical name of the brain structure.

## Value

a numeric matrix, one row per surface vertex, one column per index of
the other matrix dimension.
