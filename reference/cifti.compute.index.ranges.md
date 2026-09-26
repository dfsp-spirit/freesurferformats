# Compute the index ranges of brain model entries.

The `IndexOffset` and `IndexCount` fields of a brain model entry say
which matrix entries it covers. They are derived from the index lists
(and from the surface sizes or the volume for a model that covers
everything), and they have to cover the dimension without gaps, so they
are always computed here rather than taken from the caller.

## Usage

``` r
cifti.compute.index.ranges(models, volume = NULL)
```

## Arguments

- models:

  list of brain model entries, see
  [`cifti.brain.model.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.surface.md).

- volume:

  a volume or `NULL`, see
  [`cifti.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.volume.md).
  Needed for a volume model that has no voxel index list (which covers
  all voxels of the volume).

## Value

the models, with `index_offset` and `index_count` set.
