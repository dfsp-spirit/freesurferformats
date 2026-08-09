# Write a binary surface label in GIFTI format.

The data will be written with intent 'NIFTI_INTENT_LABEL' and as
datatype 'NIFTI_TYPE_INT32'. The label table will include entries
'positive' (label value 0), and 'negative' (label value 1).

## Usage

``` r
write.fs.label.gii(filepath, vertex_indices, num_vertices_in_surface)
```

## Arguments

- filepath:

  string, the full path of the output GIFTI file.

- vertex_indices:

  integer vector, the vertex indices which are part of the label
  (positive). All others not listed, up to num_vertices_in_surface, will
  be set to be negative.

- num_vertices_in_surface:

  integer, the total number of vertices in the surface mesh. A GIFTI
  label is more like a mask/an annotation, so we need to know the number
  of vertices.

## Value

format, string. The format that was used to write the data: "gii".

## See also

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.curv.md),
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md),
[`read.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.weight.md),
[`write.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.curv.md),
[`write.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.mgh.md),
[`write.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.md),
[`write.fs.morph.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.asc.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md),
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md),
[`write.fs.morph.smp()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.smp.md),
[`write.fs.morph.txt()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.txt.md),
[`write.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.md),
[`write.fs.weight.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.asc.md)

Other gifti writers:
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md)

## Examples

``` r
label <- c(1L, 23L, 44L)
# the positive vertex indices
outfile <- tempfile(fileext = ".gii")
write.fs.label.gii(outfile, label, 50L)
```
