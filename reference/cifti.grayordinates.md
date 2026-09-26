# Get the brainordinate table of a CIFTI-2 file.

Get the mapping from the matrix indices of one dimension to the
individual surface vertices and volume voxels a CIFTI-2 file contains.
This is the per-index version of the table returned by
[`cifti.structures`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structures.md),
which reports index ranges instead of single indices. It is useful to
find out which vertex or voxel a data row belongs to, or to find the
data index of a vertex or voxel.

Note that the same vertex or voxel can only appear once per dimension
(the matrix has one entry per brainordinate), but a structure can be
split over several brain model entries, and the vertices of a
grayordinates file are a subset of the vertices of the surface it refers
to.

## Usage

``` r
cifti.grayordinates(cii, dim = 0L)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- dim:

  integer, the matrix dimension to get the brain models for. CIFTI-2
  files have two dimensions, so this is usually 0 (Workbench calls it
  the ROW dimension) or 1 (the COLUMN dimension). See
  `read.cifti.header` for details on the dimensions.

## Value

a data.frame with one row per index of the matrix dimension and the
columns 'index' (integer, 0-based matrix index), 'structure' (character
string, the brain structure as spelled in the file), 'structure_short'
(without the `CIFTI_STRUCTURE_` prefix), 'model_type' ('SURFACE' or
'VOXELS'), 'vertex_index' (integer, 0-based index of the vertex in the
surface for surface models, `NA` for volume models) and 'i', 'j', 'k'
(integer, 0-based voxel indices for volume models, `NA` for surface
models).

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
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
cifti_file <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
cii <- read.cifti.header(cifti_file)
grayordinates <- cifti.grayordinates(cii, dim = 1L)
head(grayordinates)
#>   index                   structure structure_short model_type vertex_index  i
#> 1     0 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            0 NA
#> 2     1 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            1 NA
#> 3     2 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            2 NA
#> 4     3 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            3 NA
#> 5     4 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            4 NA
#> 6     5 CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            5 NA
#>    j  k
#> 1 NA NA
#> 2 NA NA
#> 3 NA NA
#> 4 NA NA
#> 5 NA NA
#> 6 NA NA
```
