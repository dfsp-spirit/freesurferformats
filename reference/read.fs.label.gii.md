# Read a label from a GIFTI label/annotation file.

Read a label from a GIFTI label/annotation file.

## Usage

``` r
read.fs.label.gii(filepath, label_value = 1L, element_index = 1L)
```

## Arguments

- filepath:

  string. Full path to the input label file.

- label_value:

  integer, the label value of interest to extract from the annotation:
  the indices of the vertices with this value will be returned. See the
  note for details.. It is important to set this correctly, otherwise
  you may accidently load the vertices which are *not* part of the
  label.

- element_index:

  positive integer, the index of the data array to return. Ignored
  unless the file contains several data arrays.

## Value

integer vector, the vertex indices of the label

## Note

A GIFTI label is more like a FreeSurfer annotation, as it assigns a
label integer (region code) to each vertex of the surface instead of
listing only the set of 'positive' vertex indices. If you are not sure
about the contents of the label file, it is recommended to read it with
[`read.fs.annot.gii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.gii.md)
instead. The `read.fs.label.gii` function only extracts one of the
regions from the annotation as a label, while
[`read.fs.annot.gii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.gii.md)
reads the whole annotation and gives you access to the label table,
which should assign region names to each region, making it clearer which
'label_value' you want.

## See also

Other label functions:
[`read.fs.label()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.md),
[`read.fs.label.native()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.native.md),
[`write.fs.label()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.md)

Other gifti readers:
[`read.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.gii.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
[`read.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.gii.md)
