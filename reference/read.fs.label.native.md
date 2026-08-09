# Read file in FreeSurfer label format

Read a mask in FreeSurfer label format. A label defines a list of
vertices (of an associated surface or morphometry file) which are part
of it. All others are not. You can think of it as binary mask. Label
files are ASCII text files, which have 5 columns (vertex index, coord1,
coord2, coord3, value), but only the vertex indices are of interest. A
label can also contain voxels, in that case the indices are -1 and the
coordinates are important.

## Usage

``` r
read.fs.label.native(
  filepath,
  return_one_based_indices = TRUE,
  full = FALSE,
  metadata = list()
)
```

## Arguments

- filepath:

  string. Full path to the input label file.

- return_one_based_indices:

  logical. Whether the indices should be 1-based. Indices are stored
  zero-based in the file, but R uses 1-based indices. Defaults to TRUE,
  which means that 1 will be added to all indices read from the file
  before returning them. Notice that for volume labels, the indices are
  negative (-1), and the coord fields contain the *positions* of the
  voxels it tkras space (**not** the voxel *indices* in a volume). If a
  file contains negative indices, they will NOT be incremented, no
  matter what this is set to.

- full:

  logical, whether to return a full object of class `fs.label` instead
  of only a vector containing the vertex indices. If TRUE, a named list
  with the following two entries is returned: 'one_based_indices':
  logical, whether the vertex indices are one-based. 'vertexdata': a
  data.frame with the following columns: 'vertex_index': integer, see
  parameter 'return_one_based_indices', 'coord1', 'coord2', 'coord3':
  float coordinates, 'value': float, scalar data for the vertex, can
  mean anything. This parameter defaults to FALSE.

- metadata:

  named list of arbitrary metadata to store in the instance, ignored
  unless the paramter `full` is TRUE.

## Value

vector of integers or `fs.label` instance (see parameter `full`). The
vertex indices from the label file. See the parameter
`return_one_based_indices` for important information regarding the start
index.

## Note

To load volume/voxel labels, you will have to set the 'full' parameter
to `TRUE`.

## See also

Other label functions:
[`read.fs.label()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.md),
[`read.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.gii.md),
[`write.fs.label()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.md)

## Examples

``` r
labelfile <- system.file("extdata", "lh.entorhinal_exvivo.label",
  package = "freesurferformats", mustWork = TRUE
)
label <- read.fs.label(labelfile)
```
