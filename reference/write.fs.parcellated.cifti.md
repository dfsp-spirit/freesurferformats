# Write a CIFTI-2 parcellated map or series file.

Write data that has one value (or one time series) per parcel to a
parcellated CIFTI-2 file: a `.pscalar` (scalar maps, e.g. a mean value
per region) or a `.ptseries` (a time series per region). The parcellated
dimension of these file types is matrix dimension 1, and the order of
the parcels is the order of the parcels axis, which comes from a
template file or from a parcels axis that you build (see
[`cifti.axis.parcels.from.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md)
for a parcellation of this package, e.g. the annotations of a FreeSurfer
atlas).

The file type is derived from the file name: a name ending in
`.pscalar.nii` gets a scalars dimension (the map names come from
`map_names`), one ending in `.ptseries.nii` a series dimension
(described by `start`, `step` and `unit`). Use
[`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md)
directly for the other parcellated types (`.pconn`, the mixed connectome
types) or for a file with a non-standard name.

## Usage

``` r
write.fs.parcellated.cifti(
  filepath,
  data,
  template = NULL,
  axes = NULL,
  map_names = NULL,
  start = 0,
  step = 1,
  unit = "SECOND",
  metadata = NULL
)
```

## Arguments

- filepath:

  character string, the path of the file to write, ending in
  `.pscalar.nii` or `.ptseries.nii`.

- data:

  numeric vector or matrix, the data: a vector with one value per parcel
  (a `.pscalar` with a single map), or a matrix with one row per map
  (for a `.pscalar`) or series point (for a `.ptseries`) and one column
  per parcel. The first matrix dimension is CIFTI matrix dimension 0,
  like for every other reader and writer of this package.

- template:

  character string (the path of a CIFTI-2 file), an `fs.cifti` or an
  `fs.cifti.data` object, or `NULL`. The parcels of this file are used,
  so pass the parcellated file the data belongs to (e.g. a `.ptseries`
  or `.pconn` of the same subject).

- axes:

  list of two axes or a single parcels axis (see
  [`cifti.axis.parcels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.md)),
  or `NULL`. Use this instead of `template` to write data for a
  parcellation that is not in a CIFTI-2 file, e.g. one built from
  annotations with
  [`cifti.axis.parcels.from.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md).

- map_names:

  character vector or `NULL`, the names of the maps of a `.pscalar`. The
  number of names has to match the number of rows of the data. Without
  this, the map names of the template are kept if it is a `.pscalar`
  with as many maps as the data.

- start:

  numeric, the value of the first series point of a `.ptseries`, see
  [`cifti.axis.series`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.series.md).

- step:

  numeric, the difference between consecutive series points.

- unit:

  character string, the unit of the series, one of 'SECOND', 'HERTZ',
  'METER' or 'RADIAN'.

- metadata:

  named character vector or named list, or `NULL`, the matrix metadata,
  see
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
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
template <- system.file("extdata", "cifti", "tiny.ptseries.nii", package = "freesurferformats")
data <- matrix(seq_len(3 * 4), nrow = 4L) # 4 series points, 3 parcels
out_file <- file.path(tempdir(), "written.ptseries.nii")
write.fs.parcellated.cifti(out_file, data, template = template, step = 0.5)
dim(read.cifti(out_file)$data)
#> [1] 4 3
```
