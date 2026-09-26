# Read selected matrix rows of a CIFTI-2 file without loading the whole matrix.

Read a few indices of CIFTI matrix dimension 0 (the rows of the data
matrix) from a large file, without ever holding the values of the rows
that are not requested in memory.
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md)
supports the same selection, but it reads the whole matrix first and
then drops the unrequested rows, which is impossible for a file that
does not fit into memory: the matrix of an HCP subject (91,282
grayordinates in both dimensions) is 33 GB. This function streams
through the file in chunks and keeps only the requested rows, so its
memory usage is the size of the result plus one chunk (32 MB by
default), regardless of the size of the file.

Note which direction of a CIFTI-2 file is the cheap one: the values of a
*cell of a row* are stored with a stride (`dim[5]` values lie between
the values of one row), while the values of a column are contiguous.
Reading rows therefore has to touch every value of the file once (it is
a single sequential pass, not a seek per value, but the I/O is the size
of the file), while selecting `columns` reads only what was asked for.
Use
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md)
with the `columns` parameter if the rows you need are the ones that a
column selection can give you, e.g. because the matrix is symmetric
(which a `.dconn` is), and use this function for the cases that need
rows: for a `.dtseries`, one row is one time point of all grayordinates,
so reading the first few time points of a 1.7 GB file with this function
needs a few KB instead of the whole file.

## Usage

``` r
read.cifti.rows(filepath, rows, columns = NULL, chunk_values = 4000000L)
```

## Arguments

- filepath:

  character string, the path to a CIFTI-2 file. Note that a CIFTI-2 file
  is a NIFTI-2 file, but its data are not a 3D volume; gzipped CIFTI
  files do not exist (the format forbids compression so that random
  access remains possible).

- rows:

  integer vector, the indices of matrix dimension 0 to read. At least
  one index has to be given; the indices are 1-based, see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).

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

- chunk_values:

  integer, the number of data values that are read from the file per
  chunk. This does not change the result, only the peak memory usage and
  the I/O granularity, so it is rarely needed: the default of 4 millions
  values corresponds to about 16 MB. The chunk size is rounded up to a
  whole number of matrix columns, and the result is the same for every
  chunk size.

## Value

a named list with the entries 'header' and 'data', see
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
The 'data' entry holds the requested rows, with the requested columns if
`columns` was given.

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
[`read.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md),
[`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md),
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
cifti_file <- system.file("extdata", "cifti", "tiny.dtseries.nii", package = "freesurferformats")
# The first two time points of all grayordinates:
first_frames <- read.cifti.rows(cifti_file, rows = 1:2)
dim(first_frames$data)
#> [1]  2 22

# A few time points and a few grayordinates:
subset <- read.cifti.rows(cifti_file, rows = 2, columns = 1:3)
subset$data
#>     CORTEX_LEFT_0 CORTEX_LEFT_1 CORTEX_LEFT_2
#> 2.5           200           201           202
```
