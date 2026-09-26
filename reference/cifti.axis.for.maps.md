# Build the axis for a set of maps.

Build the axis for a set of maps.

## Usage

``` r
cifti.axis.for.maps(map_names, nx, template_map_axis = NULL)
```

## Arguments

- map_names:

  character vector or `NULL`, the map names.

- nx:

  integer, the number of maps in the data.

- template_map_axis:

  the axis of the template file for this dimension, or `NULL`. If it has
  as many maps as the data, its map names and the per-map metadata (e.g.
  the palette information) are kept.

## Value

an axis, see
[`cifti.axis.scalars`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.scalars.md).
