# Create the CIFTI-2 XML metadata for a set of axes.

Build the CIFTI-2 XML document (the part of a CIFTI-2 file that
describes what the data matrix contains) from a set of axes. This is the
inverse of the XML parsing that
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md)
performs: pass the result of this function to
[`cifti.parse.xml`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parse.xml.md)
or to
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md)
(after storing it in a file) and you get back the axes you put in.

## Usage

``` r
cifti.header.from.axes(axes, metadata = NULL)
```

## Arguments

- axes:

  list of axes, one per matrix dimension, as created by the
  `cifti.axis.*()` functions or by
  [`cifti.axis.from.template`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.from.template.md).
  If the list is named, the names have to be the matrix dimensions ('0'
  and '1'), otherwise the axes are assigned to the dimensions in the
  order in which they are given.

- metadata:

  named character vector or named list or `NULL`, the metadata of the
  matrix (e.g. the provenance information), as name/value pairs.

## Value

character string, the CIFTI-2 XML document.

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
axis_series <- cifti.axis.series(4L, start = 0, step = 2.5)
axis_brain <- cifti.axis.brain.models(list(
  cifti.brain.model.surface("lh", 10L),
  cifti.brain.model.surface("rh", 12L)))
xml <- cifti.header.from.axes(list(axis_series, axis_brain))
cat(substr(xml, 1, 200))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <CIFTI Version="2">
#>   <Matrix>
#>     <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES" NumberOfSeriesPoints="4" SeriesS
```
