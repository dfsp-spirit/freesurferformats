# Read CIFTI-2 header (XML metadata).

Read the metadata of a CIFTI version 2 file, i.e. the XML document that
describes everything except the actual data values: the matrix
dimensions and their meaning, the brain models (surface vertices and
volume voxels), the parcels, the series information, and the label
tables. CIFTI-2 files are NIFTI-2 files that store this XML in a NIFTI
v2 header extension with the extension code 32. Use `read.cifti` to read
the data values as well (not implemented yet), or the accessor functions
[`cifti.structures`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structures.md),
[`cifti.parcels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcels.md),
[`cifti.series.info`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.series.info.md)
and
[`cifti.label.table`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md)
to inspect the result.

## Usage

``` r
read.cifti.header(filepath)
```

## Arguments

- filepath:

  character string, the path to a CIFTI-2 file (usually one of
  `.dscalar.nii`, `.dtseries.nii`, `.dlabel.nii`, `.dconn.nii`,
  `.pscalar.nii`, `.ptseries.nii`, `.pconn.nii`, `.dpconn.nii` or
  `.pdconn.nii`). Note that this is not a NIFTI file, despite the `.nii`
  part. Gzipped CIFTI files are not supported, because the CIFTI-2
  format forbids compression.

## Value

an `fs.cifti` object, a named list with the entries: 'filepath' (the
file path), 'niiheader' (the NIFTI-2 header as returned by
[`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)),
'version' (the CIFTI version, always '2' for files that can be read),
and 'matrix', a named list with the entries 'metadata' (the matrix-level
metadata, a named list of character strings, in file order; the names
may repeat) and 'indices_maps' (a list of the `MatrixIndicesMap`
elements, see below), plus 'dim_sizes' (the integer sizes of the matrix
dimensions; these are stored in entries 5, 6, ... of the `dim` field of
the NIFTI-2 header, and R index vectors are 1-based, so the 6th entry of
`dim` holds matrix dimension 0).

Each element of `indices_maps` is a named list with the entries: 'dims'
(integer vector, the 0-based matrix dimensions this mapping applies to;
it has several entries for files like a `.dconn`, where one mapping
describes both dimensions), 'applies_to' (the same as a character
string, as found in the file), 'type' (character string, one of
'CIFTI_INDEX_TYPE_BRAIN_MODELS', 'CIFTI_INDEX_TYPE_PARCELS',
'CIFTI_INDEX_TYPE_SERIES', 'CIFTI_INDEX_TYPE_SCALARS' or
'CIFTI_INDEX_TYPE_LABELS'), 'size' (integer vector, the size of the
dimension(s) from 'dims'), 'series' (a list with entries
'number_of_series_points', 'start', 'step', 'exponent' and 'unit', for
series mappings; `NULL` otherwise), 'surfaces' (a list of lists with
entries 'brain_structure' and 'surface_number_of_vertices'), 'volumes'
(a list of lists with entries 'dimensions' (integer vector of length 3),
'meter_exponent' and 'transformation_matrix' (4x4 numeric matrix,
row-major as in the file, mapping 0-based voxel indices to coordinates
in units of `10^meter_exponent`)), 'brain_models' (a list of lists with
entries 'index_offset' (0-based), 'index_count', 'model_type',
'brain_structure', 'surface_number_of_vertices' (surfaces only, `NA`
otherwise), 'vertex_indices' (0-based integer vector, or `NULL` if all
vertices of the surface are used) and 'voxel_indices_ijk' (an n x 3
integer matrix of 0-based voxel indices, or `NULL`) ), 'parcels' (a list
of lists with entries 'index' (0-based position in the list), 'name',
'vertices' (named list of 0-based vertex index vectors, named by the
canonical brain structure name, e.g. 'CORTEX_LEFT') and
'voxel_indices_ijk'), and 'named_maps' (a list of lists with entries
'name', 'metadata' and 'labels'; 'labels' is a data.frame with the
columns 'key', 'red', 'green', 'blue', 'alpha', 'label', 'x', 'y' and
'z', see
[`cifti.label.table`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md)).

## Note

This function performs structural validation of the XML and stops with a
descriptive error if the file is not a CIFTI-2 file, if the XML is
inconsistent (e.g., index ranges that do not cover the data, or a number
of parcels that does not match the matrix dimension), or if a file in
the outdated CIFTI-1 format is passed (which has to be converted first,
see the error message).

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
cii
#> CIFTI-2 file '/home/runner/work/_temp/Library/freesurferformats/extdata/cifti/tiny.dscalar.nii' (NIFTI-2, 4 data values per matrix entry).
#> Matrix dimensions: 4 x 22 (CIFTI dimension 0, 1).
#>   Dimension 0: scalars (maps), 4 per dimension.
#>     4 named maps.
#>   Dimension 1: brain models (surface vertices and/or volume voxels), 22 per dimension.
#>     2 brain model entries: CORTEX_LEFT, CORTEX_RIGHT
cii$matrix$indices_maps[[1]]$type
#> [1] "CIFTI_INDEX_TYPE_SCALARS"
cifti.structures(cii, dim = 1L)
#>                      structure structure_short model_type index_offset
#> 1  CIFTI_STRUCTURE_CORTEX_LEFT     CORTEX_LEFT    SURFACE            0
#> 2 CIFTI_STRUCTURE_CORTEX_RIGHT    CORTEX_RIGHT    SURFACE           10
#>   index_count surface_number_of_vertices
#> 1          10                         10
#> 2          12                         12
```
