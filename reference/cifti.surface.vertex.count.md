# Get the number of vertices of one surface of a CIFTI-2 mapping.

Get the size of the complete surface a brain model refers to. The
`Surface` elements of the mapping are used if the file has them,
otherwise the `SurfaceNumberOfVertices` attributes of the brain models
are used: the `Surface` elements are optional, and the files written by
Connectome Workbench (including the official CIFTI-2 example files) do
not contain any, they report the surface size in the brain models only.

## Usage

``` r
cifti.surface.vertex.count(map, structure)
```

## Arguments

- map:

  the indices map, see
  [`cifti.map.for.dim`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.map.for.dim.md).

- structure:

  character string, the canonical name of the brain structure.

## Value

integer, the number of vertices, or `NA_integer_` if the structure has
no surface model in this mapping.
