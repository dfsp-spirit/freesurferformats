# Write a parcellation to a CIFTI-2 `.dlabel` file.

Write per-vertex label keys (one per vertex of the complete surface, per
structure) to a CIFTI-2 dense label file. This is the inverse of
[`read.fs.parcellation.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.parcellation.cifti.md).

## Usage

``` r
write.fs.parcellation.cifti(
  filepath,
  data,
  template = NULL,
  structure = NULL,
  label_table = NULL,
  map_name = NULL,
  metadata = NULL
)
```

## Arguments

- filepath:

  character string, the path of the file to write. The name should end
  with `.dscalar.nii`.

- data:

  integer vector, matrix or named list, the label keys per vertex, see
  `data` in
  [`write.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md).
  The values have to be the label keys of the label table (they are
  written as integers, and a key of 0 usually means unknown).

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

- label_table:

  data.frame or `NULL`, the label table, see
  [`cifti.axis.labels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.labels.md):
  the columns 'key', 'red', 'green', 'blue', 'alpha' (in the range 0
  to 1) and 'label'. The colortable of an `fs.annot` instance (see
  [`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md))
  is accepted as well, its colors are then divided by 255. Without a
  label table the file still stores the keys, but nothing in it explains
  what they mean (Connectome Workbench writes one, and its label files
  rely on it).

- map_name:

  character string or `NULL`, the name of the label map.

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
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
template <- system.file("extdata", "cifti", "tiny.dlabel.nii", package = "freesurferformats")
keys <- read.fs.parcellation.cifti(template, "lh")
label_table <- read.fs.parcellation.cifti(template, "lh", with_label_table = TRUE)$label_table
out_file <- file.path(tempdir(), "written.dlabel.nii")
write.fs.parcellation.cifti(out_file, keys, template = template, structure = "lh",
                            label_table = label_table)
table(read.fs.parcellation.cifti(out_file, "lh"))
#> 
#> 0 1 2 3 
#> 1 3 3 3 
```
