# Write annotation to GIFTI file.

Write an annotation to a GIFTI XML file.

## Usage

``` r
write.fs.annot.gii(filepath, annot)
```

## Arguments

- filepath:

  string, path to the output file.

- annot:

  fs.annot instance, an annotation.

## Note

This function does not write a GIFTI file that is valid according to the
specification: it stores extra color data in the Label nodes, and there
is more than one Label in the LabelTable node.

## See also

Other atlas functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

Other gifti writers:
[`write.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.gii.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.surface.gii.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load annotation
annot_file <- system.file("extdata", "lh.aparc.annot.gz",
  package = "freesurferformats",
  mustWork = TRUE
)
annot <- read.fs.annot(annot_file)
# New method: write the annotation instance:
write.fs.annot.gii(tempfile(fileext = ".annot"), annot)
} # }
```
