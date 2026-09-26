# Determine the structures of per-vertex data.

Determine the structures of per-vertex data.

## Usage

``` r
cifti.data.structures(data, structure = NULL)
```

## Arguments

- data:

  the data, see
  [`write.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md).

- structure:

  character string or `NULL`, the structure for the non-list case.

## Value

character vector of canonical structure names, or `NULL` if the data are
not a named list and no structure was given.
