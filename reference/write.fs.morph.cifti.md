# Write morphometry data to a CIFTI-2 `.dscalar` file.

Write a per-vertex data vector (or several of them, as the maps of the
file) to a CIFTI-2 dense scalar file. This is the inverse of
[`read.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.cifti.md):
the data are given for the *complete* surface, and the vertices that the
file contains are selected with the mapping of a template file (which is
what a grayordinates file needs, since it leaves out the medial wall).

## Usage

``` r
write.fs.morph.cifti(
  filepath,
  data,
  template = NULL,
  structure = NULL,
  map_names = NULL,
  metadata = NULL
)
```

## Arguments

- filepath:

  character string, the path of the file to write. The name should end
  with `.dscalar.nii`.

- data:

  numeric vector or matrix or named list, the per-vertex data:

  - a vector: one value per vertex of the complete surface of one
    structure,

  - a matrix: one row per vertex of the complete surface, one column per
    map,

  - a named list (e.g. `list(lh = ..., rh = ...)`): a vector or matrix
    as above per structure.

  The vertex order is the order of the surface mesh, which is also the
  order in which
  [`read.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.cifti.md)
  returns the data.

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

- map_names:

  character vector or `NULL`, the names of the maps. The number of names
  has to match the number of maps in the data. Without this, the map
  names of the template file are kept if it has as many maps as the
  data.

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
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
out_file <- file.path(tempdir(), "written.dscalar.nii")
data <- read.fs.morph.cifti(template, "lh")
data[2] <- 555 # change one value
write.fs.morph.cifti(out_file, data, template = template, structure = "lh")
read.fs.morph.cifti(out_file, "lh")[1:3]
#> [1] 100 555 102
```
