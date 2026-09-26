# Write a CIFTI-2 file.

Write a data matrix and its axes to a CIFTI-2 file, i.e. a NIFTI-2 file
with the CIFTI-2 XML in a header extension. The axes describe what the
matrix dimensions contain; they can be given explicitly (see the
`cifti.axis.*()` functions), taken from a template file (recommended for
real data, see
[`cifti.axis.from.template`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.from.template.md)),
or both.

The file type (and thus the NIFTI intent code that identifies it)
follows from the axes: a scalars dimension and a brain model dimension
make a `.dscalar` file, a series dimension and a brain model dimension a
`.dtseries` file, and so on. If the file name has one of the standard
CIFTI extensions, the axes have to match it: a file named `.pdconn.nii`
whose dimensions are not (brain models, parcels) is an error, not a
warning, because the file name is the only hint that other software has
about the content.

## Usage

``` r
write.cifti(filepath, data, axes = NULL, template = NULL, metadata = NULL)
```

## Arguments

- filepath:

  character string, the path of the file to write.

- data:

  numeric or integer matrix or array, the data. The dimensions have to
  match the sizes of the axes, and the order is the same as for
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md):
  the first array dimension is CIFTI matrix dimension 0. A plain vector
  is interpreted as an array of that length (which is what you want for
  a file with a single map, e.g. a `.dscalar` with one map).

- axes:

  list of axes, one per matrix dimension, or `NULL` (in which case
  `template` has to be given). See
  [`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md)
  for the naming rules.

- template:

  character string (the path of a CIFTI-2 file), an `fs.cifti` or an
  `fs.cifti.data` object or `NULL`. The axes of this file are used for
  the dimensions that `axes` does not describe, and also for the
  metadata if `metadata` is `NULL`.

- metadata:

  named character vector or named list, or `NULL`, the matrix metadata
  to write. The default writes the metadata of the template file, if
  there is one, so that the provenance of the source data is kept.

## Value

the axes that were written, invisibly. The file is written as a side
effect.

## Note

The data are written as 32 bit floating point values, which is what the
reference implementations write for CIFTI-2 files (including for label
files, whose keys are small integers). Values that need more than about
7 significant digits are rounded.

## See also

Other cifti functions:
[`cifti.axis.brain.models()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.brain.models.md),
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
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
axis_brain <- cifti.axis.brain.models(list(
  cifti.brain.model.surface("lh", 10L),
  cifti.brain.model.surface("rh", 12L)))
data_matrix <- matrix(1:22, nrow = 1L)
out_file <- file.path(tempdir(), "tiny_written.dscalar.nii")
write.cifti(out_file, data_matrix, axes = list(cifti.axis.scalars("my data"), axis_brain))
if (FALSE) { # \dontrun{
# Write data with the mapping of an existing file (recommended for real data):
template <- "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii"
data_matrix <- read.cifti(template)$data
write.cifti("copy.dscalar.nii", data_matrix, template = template)
} # }
```
