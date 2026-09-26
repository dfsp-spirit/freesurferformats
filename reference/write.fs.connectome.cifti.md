# Write a CIFTI-2 connectome file.

Write a connectome matrix to a CIFTI-2 file: a dense connectome
(`.dconn`), a parcellated connectome (`.pconn`), or one of the mixed
types (`.pdconn`, `.dpconn`) whose two dimensions hold brainordinates
and parcels. The file type follows from the axes (and has to match the
file name, see
[`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md)),
and the mapping of the two dimensions has to be given: it cannot be
derived from the matrix, and guessing it (e.g. from the size of the
matrix) would silently produce a file whose rows and columns describe
the wrong brain regions.

Use `template` for the normal case, i.e. to write a connectome of the
brainordinates or parcels of an existing file: this is how a `.dconn` of
a subject gets the mapping of that subject, and how a `.pconn` gets the
parcels of the `.ptseries` it was computed from. A template with a
single brainordinate dimension (a `.dscalar`, `.dtseries` or `.dlabel`,
i.e. the files that actually exist for a subject) is accepted as well:
its mapping is then used for both dimensions of the connectome, which is
what those files describe. Use `axes` if the mapping has to be built,
e.g. a parcels axis from annotations (see
[`cifti.axis.parcels.from.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md)).

The complete matrix is written: a connectome file stores both halves of
the matrix and its diagonal, so the symmetry of the matrix is not
exploited anywhere, and the matrix of a `.dconn` can be huge (33 GB for
the 91,282 grayordinates of an HCP subject, which is more than any
implementation can hold in memory). Writing such a file is not supported
yet; a connectome of a reduced set of brainordinates can be written.

## Usage

``` r
write.fs.connectome.cifti(
  filepath,
  data,
  template = NULL,
  axes = NULL,
  metadata = NULL
)
```

## Arguments

- filepath:

  character string, the path of the file to write. The name should be
  one of `.dconn.nii`, `.pconn.nii`, `.pdconn.nii` or `.dpconn.nii` (the
  file type is derived from the axes, and the name has to agree with
  it).

- data:

  numeric matrix, the connectome: the first matrix dimension (the rows)
  is CIFTI matrix dimension 0, which the axes describe. An
  `fs.connectome` instance (see
  [`read.fs.connectome.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md))
  is accepted as well, in which case its data and its header are used.

- template:

  character string (the path of a CIFTI-2 file), an `fs.cifti` or an
  `fs.cifti.data` object, or `NULL`. The axes of this file are used; a
  file with a single brainordinate dimension has that mapping used for
  both dimensions of the connectome.

- axes:

  list of two axes (see
  [`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md))
  or a single axis that is used for both dimensions, or `NULL`. Use this
  instead of `template` to write a file with a mapping that is not in a
  file, e.g. a parcels axis built with
  [`cifti.axis.parcels.from.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md).

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
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
pconn_file <- system.file("extdata", "cifti", "tiny.pconn.nii", package = "freesurferformats")
conn <- read.fs.connectome.cifti(pconn_file)
out_file <- file.path(tempdir(), "written.pconn.nii")
# The values are squared, the mapping is the one of the original file:
write.fs.connectome.cifti(out_file, conn$data^2, template = pconn_file)
read.fs.connectome.cifti(out_file)$data[1:2, 1:2]
#>          PARCEL_A PARCEL_B
#> PARCEL_A        1        1
#> PARCEL_B        1        1

if (FALSE) { # \dontrun{
# The grayordinates of a subject are the same in all its dense files, so a dtseries
# can define the mapping of the dconn:
write.fs.connectome.cifti("sub-01_dconn.nii", connectivity_matrix,
                          template = "sub-01_task-rest_dtseries.nii")
} # }
```
