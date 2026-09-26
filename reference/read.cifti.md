# Read a CIFTI-2 file.

Read a CIFTI-2 file, i.e. its XML metadata (see
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md))
and its data values. The data are returned as an array in the order in
which they are stored in the file: the first array dimension is CIFTI
matrix dimension 0 (which Connectome Workbench calls the ROW dimension),
the second one is matrix dimension 1 (the COLUMN dimension). This is the
same order that other CIFTI implementations use for their data arrays.

For a dense file like a `.dscalar`, dimension 0 holds the scalars (maps)
and dimension 1 the brainordinates (surface vertices and volume voxels),
so the data matrix has one row per map and one column per grayordinate.
For a `.dtseries`, dimension 0 holds the series, so the matrix has one
row per series point. The functions
[`cifti.series.info`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.series.info.md)
and
[`cifti.map.for.dim`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.map.for.dim.md)
tell you what a dimension is; never conclude it from the file name. Use
[`cifti.structure.data`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md)
to extract the data of a brain structure, which is the more convenient
interface for most purposes and the one that the `read.fs.*.cifti`
functions are built on.

Reading a file twice (once for the header, once for the data) is not
necessary to extract a single structure, but it is what this function
does; use it if you need more than one structure or the raw matrix.

## Usage

``` r
read.cifti(filepath, rows = NULL, columns = NULL)
```

## Arguments

- filepath:

  character string, the path to a CIFTI-2 file. Note that a CIFTI-2 file
  is a NIFTI-2 file, but its data are not a 3D volume; gzipped CIFTI
  files do not exist (the format forbids compression so that random
  access remains possible).

- rows:

  integer vector or `NULL`, the indices of matrix dimension 0 to read.
  Indices are 1-based, like everywhere else in R, and they refer to the
  rows of the returned array, i.e. to the first CIFTI matrix dimension.
  Use `NULL` (the default) to read all of them. Note that this selection
  is applied after reading the whole matrix: use
  [`read.cifti.rows`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md)
  if the file is too large for that.

- columns:

  integer vector or `NULL`, the indices of matrix dimension 1 to read,
  i.e. the columns of the returned array. This is the matrix dimension
  that holds the brainordinates of a dense file (the grayordinates of a
  `.dscalar`, `.dtseries` or `.dlabel`), and it is also one of the two
  dimensions that hold them in a connectome file like a `.dconn`.
  Selecting columns avoids reading the rest of the file, which is the
  only practical way to read a small part of a huge file like a `.dconn`
  (9 to 38 GB): for a 2-dimensional matrix, one contiguous block of the
  file holds all values of one column. Note that `rows` does not have
  this property, selecting rows still reads all values of the file.

## Value

a named list with the entries 'header' (an `fs.cifti` object, see
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md))
and 'data' (the numeric or integer array). The array dimensions are
named with the axis labels of the corresponding matrix dimensions, see
[`cifti.dim.labels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.labels.md).
The object has class 'fs.cifti.data'.

## Note

The generic readers
[`read.fs.morph`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md)
and
[`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
dispatch on the file name and would therefore match a CIFTI-2 file
(which is a NIFTI-2 file), so they detect this case and stop with a
pointer to this function and the other CIFTI readers instead of
returning the values of the matrix in an order that means nothing.

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
cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
cii <- read.cifti(cifti_file)
dim(cii$data)
#> [1]  4 22
# The first 3 grayordinates of the first map:
cii$data[1, 1:3]
#> CORTEX_LEFT_0 CORTEX_LEFT_1 CORTEX_LEFT_2 
#>           100           101           102 
```
