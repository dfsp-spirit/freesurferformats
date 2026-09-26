# Write time series data to a CIFTI-2 `.dtseries` file.

Write a per-vertex time series (or several structures at once) to a
CIFTI-2 dense series file. This is the inverse of
[`read.fs.series.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.series.cifti.md).

## Usage

``` r
write.fs.series.cifti(
  filepath,
  data,
  template = NULL,
  structure = NULL,
  start = 0,
  step = 1,
  unit = "SECOND",
  metadata = NULL
)
```

## Arguments

- filepath:

  character string, the path of the file to write. The name should end
  with `.dscalar.nii`.

- data:

  numeric matrix or named list, the data: a matrix with one row per
  vertex of the complete surface and one column per series point (time
  point), or a named list of such matrices per structure.

- template:

  character string (the path of a CIFTI-2 file), an `fs.cifti` or an
  `fs.cifti.data` object, or `NULL`. The grayordinate mapping of this
  file is used, so pass the file the data comes from (e.g. an HCP file
  of the same subject): the mapping of such a file cannot be invented.
  Without a template, the file covers *all* vertices of the surfaces,
  and `structure` has to be given.

- structure:

  character string or `NULL`, the brain structure the data belongs to
  (e.g. `'lh'`), needed if `data` is not a named list and there is no
  template to take the structures from.

- start:

  numeric, the value of the first series point, see
  [`cifti.axis.series`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.series.md).

- step:

  numeric, the difference between consecutive series points.

- unit:

  character string, the unit of the series, one of 'SECOND', 'HERTZ',
  'METER' or 'RADIAN'.

- metadata:

  named character vector or `NULL`, the matrix metadata, see
  [`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md).
  The default keeps the metadata of the template file.

## Value

the axes that were written, invisibly.

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
[`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md),
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md)

## Examples

``` r
template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
series <- matrix(seq_len(10 * 3), nrow = 10L) # 10 vertices (lh), 3 time points
out_file <- file.path(tempdir(), "written.dtseries.nii")
write.fs.series.cifti(out_file, series, template = template, structure = "lh", step = 0.72)
dim(read.fs.series.cifti(out_file, "lh"))
#> [1] 10  3
```
