# Create the Surface elements of an axis.

Create the Surface elements of an axis.

## Usage

``` r
cifti.axis.surfaces(surfaces)
```

## Arguments

- surfaces:

  named integer vector or `NULL`, the number of vertices per brain
  structure, see
  [`cifti.axis.brain.models`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.brain.models.md).

## Value

a list of lists with the entries 'brain_structure' and
'surface_number_of_vertices', or `NULL`.
