# Construct a brain atlas from a colortable (LUT) file and a per-vertex label file.

Construct a brain atlas (surface annotation) from a FreeSurfer ASCII
colortable lookup table (LUT) file and a text file that assigns a label
(struct index) to each vertex of a brain surface. This is useful when an
atlas is distributed as two text files: a colortable (LUT) plus a
per-vertex label assignment, for example the cortical atlases bundled
with the Python package 'yabplot' (files like 'aparc_LUT.txt' and
'aparc_conte69.csv').

## Usage

``` r
atlas.from.lut.and.csv(
  lut_file,
  csv_file = NULL,
  label_indices = NULL,
  num_vertices = NULL,
  unknown_label_name = "unknown",
  unknown_index = 0L,
  include_unknown_in_colortable = TRUE
)
```

## Arguments

- lut_file:

  string, path to a colortable file in FreeSurfer ASCII LUT format (see
  [`read.fs.colortable`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md)).
  The file must contain the columns 'struct_index', 'struct_name', 'r',
  'g', 'b', 'a', as in the 'FreeSurferColorLUT.txt' files. The struct
  index is the region identifier used in the label file.

- csv_file:

  string, path to a text file that assigns a label to each vertex. The
  file must contain one integer per line: the struct index (label ID) of
  the region for that vertex, as defined in the first column of the LUT
  file. See parameter `unknown_index` for how unlabeled vertices are
  encoded. If you already have the indices in memory, use parameter
  `label_indices` instead.

- label_indices:

  integer vector, an alternative to parameter 'csv_file'. The per-vertex
  struct indices, one per vertex. If given, 'csv_file' is ignored.

- num_vertices:

  integer, optional. The expected number of vertices. If given, it must
  match the number of label indices read from 'csv_file' or
  'label_indices'.

- unknown_label_name:

  string, the name to use for unlabeled vertices (e.g., the medial
  wall). Defaults to 'unknown'.

- unknown_index:

  integer, the struct index that is used in the label file to mark
  unlabeled vertices. Defaults to 0.

- include_unknown_in_colortable:

  logical, whether to add a region for unlabeled vertices to the
  colortable of the returned annotation, in case the LUT file does not
  already contain a region with the 'unknown_index'. Defaults to TRUE.
  This ensures that vertices without a valid label (e.g., the medial
  wall) get a proper region name and color in the returned annotation.

## Value

an 'fs.annot' instance with class 'fs.annot', see
[`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md)
for the structure. Can be written to a FreeSurfer annotation file with
[`write.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md).

## See also

Other atlas functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.atlas.to.lut.and.csv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.atlas.to.lut.and.csv.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example for the yabplot cortical atlas files (LUT + per-vertex indices):
annot <- atlas.from.lut.and.csv("aparc_LUT.txt", "aparc_conte69.csv")
write.fs.annot("lh.aparc.annot", fs.annot = annot)
} # }
```
