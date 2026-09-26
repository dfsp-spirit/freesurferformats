# Prepare per-vertex data for writing to a CIFTI-2 file.

Common work of the user-facing CIFTI-2 writers: turn data that are given
for the complete surface into the matrix of a dense CIFTI-2 file, i.e.
one row per map (or series point, or label map) and one column per
brainordinate, in the order of the grayordinates of the file.

## Usage

``` r
cifti.prepare.surface.data(
  data,
  structure = NULL,
  template = NULL,
  what = "data"
)
```

## Arguments

- data:

  numeric vector or matrix, or a named list of them, the per-vertex
  data.

- structure:

  character string or `NULL`, the structure name for the non-list case.

- template:

  character string, `fs.cifti`, `fs.cifti.data` or `NULL`, the template.

- what:

  character string, a description of the data, used in error messages.

## Value

a named list with the entries 'matrix' (the data matrix to write),
'axes' (the axes, with the brainordinate axis filled in and the other
axis left `NULL`), 'template' (the template object or `NULL`) and
'template_map_axis' (the axis of the template for the other matrix
dimension, or `NULL`).
