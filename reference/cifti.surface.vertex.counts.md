# Get the number of vertices of the surfaces declared in a CIFTI mapping.

Get the number of vertices of the surfaces declared in a CIFTI mapping.

## Usage

``` r
cifti.surface.vertex.counts(map)
```

## Arguments

- map:

  a parsed indices map, see
  [`cifti.parse.indices.map`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parse.indices.map.md).

## Value

named integer vector, the number of vertices per canonical brain
structure short name (e.g. 'CORTEX_LEFT'). Empty if the mapping declares
no surfaces.
