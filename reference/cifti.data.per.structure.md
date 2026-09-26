# Get the per-structure data of one structure.

Get the per-structure data of one structure.

## Usage

``` r
cifti.data.per.structure(data, structure_name, what)
```

## Arguments

- data:

  the data, see
  [`write.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md).

- structure_name:

  character string, the canonical structure name.

- what:

  character string, a description of the data, used in error messages.

## Value

a matrix with one row per vertex of the complete surface and one column
per map.
