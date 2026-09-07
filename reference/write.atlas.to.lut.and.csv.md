# Write a brain atlas to a colortable (LUT) file and a per-vertex label file.

Write a brain atlas (surface annotation) to two text files: a FreeSurfer
ASCII colortable lookup table (LUT) file and a per-vertex label file.
This is the inverse of
[`atlas.from.lut.and.csv`](https://dfsp-spirit.github.io/freesurferformats/reference/atlas.from.lut.and.csv.md),
and is useful for exporting an annotation in the simple text format used
e.g. by the Python package 'yabplot' (files like 'aparc_LUT.txt' and
'aparc_conte69.csv'). If the annotation was created by
[`atlas.from.lut.and.csv`](https://dfsp-spirit.github.io/freesurferformats/reference/atlas.from.lut.and.csv.md),
the original per-vertex label indices are stored in its metadata and are
used for the export, which makes it lossless. Otherwise, the indices are
recovered by matching the per-vertex color codes against the colortable,
which is ambiguous if several regions share a color code (e.g., the
left/right pairs of the Desikan-Killiany 'aparc' atlas).

## Usage

``` r
write.atlas.to.lut.and.csv(fs.annot, lut_file, csv_file, unknown_index = 0L)
```

## Arguments

- fs.annot:

  an annotation, as returned by
  [`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md)
  or
  [`atlas.from.lut.and.csv`](https://dfsp-spirit.github.io/freesurferformats/reference/atlas.from.lut.and.csv.md).
  Must have a colortable (an entry named 'colortable_df').

- lut_file:

  string, path to the output colortable file. Will be written in
  FreeSurfer ASCII LUT format, see
  [`write.fs.colortable`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md).

- csv_file:

  string, path to the output per-vertex label file. Will contain one
  integer per line: the struct index (label ID) of the region for that
  vertex, as defined in the first column of the LUT file. Unlabeled
  vertices (e.g., the medial wall) are encoded with the struct index
  given in parameter `unknown_index`.

- unknown_index:

  integer, the struct index to use for unlabeled vertices (i.e.,
  vertices whose label code has no entry in the colortable). Defaults to
  0.

## Value

named list with the following entries: 'lut_file' and 'csv_file' (the
paths to the files that were written), 'num_vertices' (integer, the
number of vertices in the per-vertex label file) and 'num_regions'
(integer, the number of regions in the LUT file).

## See also

Other atlas functions:
[`atlas.from.lut.and.csv()`](https://dfsp-spirit.github.io/freesurferformats/reference/atlas.from.lut.and.csv.md),
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

## Examples

``` r
if (FALSE) { # \dontrun{
annot <- read.fs.annot("lh.aparc.annot")
write.atlas.to.lut.and.csv(annot, "myatlas_LUT.txt", "myatlas_vertices.csv")
# read it back into an annotation:
annot2 <- atlas.from.lut.and.csv("myatlas_LUT.txt", "myatlas_vertices.csv")
} # }
```
