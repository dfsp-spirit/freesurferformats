# Read a CIFTI-2 connectome file.

Read a CIFTI-2 file whose two matrix dimensions both describe
brainordinates or parcels, i.e. a dense connectome (`.dconn`), a
parcellated connectome (`.pconn`) or one of the mixed types (`.pdconn`,
`.dpconn`). The result contains the matrix and the parcels or
brainordinates its rows and columns belong to; this is what makes a
connectome file usable, since the numbers alone do not say which pair of
regions a value describes.

Both values of a connectome are stored in the file (the matrix is not
symmetric on disk, and the diagonal is stored as well), so reading a
`.pconn` gives a square matrix with the number of parcels as its number
of rows and columns. A real `.dconn` (an HCP subject has 91,282
grayordinates, i.e. 8.3 billion values, 33 GB) can not be read into
memory at all: use the `rows` and `columns` parameters to read only the
part you need, which is a contiguous block of the file for each
requested column.

## Usage

``` r
read.fs.connectome.cifti(filepath, rows = NULL, columns = NULL)
```

## Arguments

- filepath:

  character string, the path of a CIFTI-2 file, see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
  An `fs.cifti` object (see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md))
  or an `fs.cifti.data` object (see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md))
  are accepted as well.

- rows:

  integer vector or `NULL`, the indices of matrix dimension 0 to read,
  see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).

- columns:

  integer vector or `NULL`, the indices of matrix dimension 1 to read.
  This is the way to read part of a file that is too large to read
  completely, see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).

## Value

a named list with class 'fs.connectome':

- 'data': numeric matrix, the connectome, with the parcels or
  brainordinates as the dimnames of its rows and columns (see
  [`cifti.dim.labels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.labels.md)),

- 'parcel_names': character vector, the names of the parcels, or `NULL`
  if the file has no parcellated dimension,

- 'parcels': data.frame with one row per parcel (the columns 'index',
  'name', 'num_vertices' and 'num_voxels', see
  [`cifti.parcels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcels.md)),
  or `NULL`,

- 'parcels_dim': integer, the matrix dimension that holds the parcels (0
  or 1), or `NA` if the file has none,

- 'grayordinates': data.frame with one row per brainordinate of the
  dense dimension (see
  [`cifti.grayordinates`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md)),
  or `NULL`,

- 'grayordinates_dim': integer, the matrix dimension that holds the
  dense brainordinates (0 or 1), or `NA` if the file has none,

- 'header': the `fs.cifti` metadata object, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

Note that the parcels and brainordinates describe *all* indices of the
dimension they belong to, not only the ones selected with `rows` or
`columns`.

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
[`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md),
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
pconn_file <- system.file("extdata", "cifti", "tiny.pconn.nii", package = "freesurferformats")
conn <- read.fs.connectome.cifti(pconn_file)
dim(conn$data)
#> [1] 3 3
conn$parcel_names
#> [1] "PARCEL_A" "PARCEL_B" "PARCEL_C"
conn$data[1:2, 1:2]
#>          PARCEL_A PARCEL_B
#> PARCEL_A        1        1
#> PARCEL_B        1        1
```
