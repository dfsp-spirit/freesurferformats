# Create a CIFTI-2 axis for brain models.

A brain model axis describes which surface vertices and volume voxels a
matrix dimension of a CIFTI-2 file contains, in the order in which they
appear in the matrix. This is the mapping of a dense file (`.dscalar`,
`.dtseries`, `.dlabel`, `.dconn`), and of the files that mix dense and
parcellated data.

The index ranges of the models are computed from the index lists, so the
models cover the dimension without gaps, which the format requires. A
structure may appear in several models (e.g. as a surface and as a
volume part in a grayordinates file), and the order of the models is the
order of the matrix indices.

## Usage

``` r
cifti.axis.brain.models(models, surfaces = NULL, volume = NULL)
```

## Arguments

- models:

  list of brain model entries, as created by
  [`cifti.brain.model.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.surface.md)
  and
  [`cifti.brain.model.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.volume.md).

- surfaces:

  named integer vector or `NULL`, the number of vertices of the complete
  surfaces the file refers to, named by brain structure (e.g.
  `c(CORTEX_LEFT = 32492, CORTEX_RIGHT = 32492)`). This is written as
  the optional `Surface` elements of the axis. Connectome Workbench does
  not write them, and the surface size is then taken from the brain
  model entries only, so this can be left at `NULL` unless the file has
  to state the surface sizes explicitly.

- volume:

  a volume, see
  [`cifti.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.volume.md),
  or `NULL`. Required if any of the models is a volume model, because
  the voxel indices of a model can not be interpreted without the voxel
  grid and the transformation matrix.

## Value

a named list with the entries 'type', 'brain_models', 'surfaces' and
'volumes', an axis to be passed to
[`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md)
or
[`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md).

## See also

Other cifti functions:
[`cifti.axis.from.template()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.from.template.md),
[`cifti.axis.labels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.labels.md),
[`cifti.axis.parcels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.md),
[`cifti.axis.parcels.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md),
[`cifti.axis.scalars()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.scalars.md),
[`cifti.axis.series()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.series.md),
[`cifti.brain.model.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.surface.md),
[`cifti.brain.model.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.volume.md),
[`cifti.dim.labels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.labels.md),
[`cifti.file.type.for.axes()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.file.type.for.axes.md),
[`cifti.grayordinates()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md),
[`cifti.header.from.axes()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md),
[`cifti.label.table()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md),
[`cifti.parcel()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcel.md),
[`cifti.parcels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcels.md),
[`cifti.series.info()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.series.info.md),
[`cifti.structure.data()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md),
[`cifti.structures()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structures.md),
[`cifti.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.volume.md),
[`print.fs.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.cifti.md),
[`print.fs.cifti.data()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.cifti.data.md),
[`print.fs.connectome()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.connectome.md),
[`read.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md),
[`read.cifti.header()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md),
[`read.cifti.rows()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md),
[`read.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md),
[`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md),
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
axis <- cifti.axis.brain.models(list(
  cifti.brain.model.surface("lh", 10L),
  cifti.brain.model.surface("rh", 12L)))
axis$type
#> [1] "CIFTI_INDEX_TYPE_BRAIN_MODELS"
```
