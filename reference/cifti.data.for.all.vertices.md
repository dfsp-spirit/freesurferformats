# Assemble the data values of all vertices for a file without a template.

Assemble the data values of all vertices for a file without a template.

## Usage

``` r
cifti.data.for.all.vertices(data, structures, what = "data")
```

## Arguments

- data:

  the data, see
  [`write.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md).

- structures:

  character vector, the canonical structure names, in file order.

- what:

  character string, a description of the data, used in error messages.

## Value

a matrix with one row per vertex (structures one after the other) and
one column per map.
