# Extract the data of one brain structure from a CIFTI-2 file.

Get the data values of a single brain structure (e.g. one hemisphere)
from a CIFTI-2 file, reconstructed for the full surface. The result has
one row per vertex of the surface (in the order of the surface, which is
the order the vertices have in the surface mesh files) and one column
per index of the other matrix dimension of the file. Vertices of the
surface that have no value in the file are reported as `NA`:
grayordinates files have a reduced mesh (the medial wall vertices are
missing), and returning the values for the complete surface is what
makes such files usable together with the standard surface meshes of a
subject.

Volume structures (a brain model of type 'CIFTI_MODEL_TYPE_VOXELS',
which subcortical structures use) cannot be expanded like this, because
the voxels a structure consists of are not a rectangular block of a
volume and are not ordered in any meaningful way. For those, the data
values are returned together with the voxel indices and the affine
transformation that maps them to coordinates, see the Value section.

## Usage

``` r
cifti.structure.data(x, structure = NULL, dim = NULL)
```

## Arguments

- x:

  an `fs.cifti.data` object, see
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
  An `fs.cifti` metadata object or the path of a CIFTI-2 file are
  accepted as well, in which case the data are read from the file.

- structure:

  character string or `NULL`, the brain structure to extract the data
  for. The usual spellings are accepted ('lh', 'CORTEX_LEFT',
  'CIFTI_STRUCTURE_CORTEX_LEFT'), see
  [`cifti.structure.canonical`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.canonical.md).
  If it is `NULL`, the data of all structures are returned.

- dim:

  integer or `NULL`, the CIFTI matrix dimension that holds the brain
  structure (the brainordinate dimension). For most files there is only
  one such dimension and this can be left at `NULL`. Files in which both
  dimensions are brain models (`.dconn` and `.pdconn`) require an
  explicit value, since they contain the same structures in both
  dimensions.

## Value

If 'structure' is a single structure: a named list with the entries
'structure' (character string, the canonical name of the structure, e.g.
'CIFTI_STRUCTURE_CORTEX_LEFT'), 'structure_short' (the name without the
prefix, e.g. 'CORTEX_LEFT'), 'model_type' (character string, one of
'SURFACE', 'VOXELS' or 'SURFACE_AND_VOXELS'), 'surface' and 'volume'. If
'structure' is `NULL`: a named list of such lists, one per structure,
named by the short structure name.

For a selection that contains a surface model, the 'surface' entry holds
a numeric (or integer) array with one row per vertex of the full surface
and the remaining dimensions of the data, with `NA` for vertices that
the file does not contain. Its dimensions beyond the first are named
like in
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
If the selection contains no surface model, 'surface' is `NULL`.

For a selection that contains a volume model, the 'volume' entry holds a
named list with the entries 'values' (an array like 'surface', but with
one row per voxel of the structure instead of per surface vertex),
'voxel_indices_ijk' (an n x 3 integer matrix of 0-based voxel indices,
in the same order as the rows of 'values'), 'dimensions' (integer vector
of length 3, the dimensions of the volume the voxel indices refer to)
and 'transformation_matrix' and 'meter_exponent' (the 4x4 row-major
matrix from the file that maps the voxel indices to coordinates in units
of `10^meter_exponent`, and the exponent). If the selection contains no
volume model, 'volume' is `NULL`.

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
cii <- read.cifti(cifti_file)
lh_data <- cifti.structure.data(cii, "lh")
dim(lh_data$surface)
#> [1] 10  4
# vertices beyond the end of the mesh in the file are NA:
sum(is.na(lh_data$surface[, 1]))
#> [1] 0

# Volume structures are returned with their voxel indices:
vol_file <- system.file("extdata", "cifti", "tiny_volume.dscalar.nii",
                        package = "freesurferformats")
vol_cii <- read.cifti(vol_file)
cerebellum <- cifti.structure.data(vol_cii, "CEREBELLUM")
cerebellum$volume$voxel_indices_ijk
#>    i j k
#> 34 1 0 0
#> 35 2 0 0
#> 36 3 0 0
#> 37 0 1 0
#> 38 1 1 0
#> 39 2 1 0
#> 40 3 1 0
#> 41 0 2 0
#> 42 1 2 0
#> 43 2 2 0
#> 44 3 2 0
#> 45 0 3 0
#> 46 1 3 0
#> 47 2 3 0
#> 48 3 3 0
#> 49 1 0 1
#> 50 2 0 1
#> 51 3 0 1
#> 52 0 1 1
#> 53 1 1 1
#> 54 2 1 1
#> 55 3 1 1
#> 56 0 2 1
#> 57 1 2 1
#> 58 2 2 1
#> 59 3 2 1
#> 60 0 3 1
#> 61 1 3 1
#> 62 2 3 1
#> 63 3 3 1
```
