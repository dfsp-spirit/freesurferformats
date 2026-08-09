# Extract color lookup table (LUT) from annotation.

Extract a colortable lookup table (LUT) from an annotation. Such a LUT
can also be read from files like
`FREESURFER_HOME/FreeSurferColorLUT.txt` or saved as a file, check the
'See Also' section below.

## Usage

``` r
colortable.from.annot(annot, compute_colorcode = FALSE)
```

## Arguments

- annot:

  An annotation, as returned by
  [`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md).
  If you want to assign specific indices, you can add a column named
  'struct_index' to the data.frame `annot$colortable_df`. If there is no
  such columns, the indices will be created automatically in the order
  of the regions, starting at zero.

- compute_colorcode:

  logical, indicates whether the unique color codes should be computed
  and added to the returned data.frame as an extra integer column named
  'code'. Defaults to FALSE.

## Value

the colortable data.frame extracted from the annotation.

## See also

Other atlas functions:
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

Other colorLUT functions:
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

## Examples

``` r
annotfile <- system.file("extdata", "lh.aparc.annot.gz",
  package = "freesurferformats", mustWork = TRUE
)
annot <- read.fs.annot(annotfile)
colortable <- colortable.from.annot(annot)
head(colortable)
#>   struct_index             struct_name   r   g   b a
#> 1            0                 unknown  25   5  25 0
#> 2            1                bankssts  25 100  40 0
#> 3            2 caudalanteriorcingulate 125 100 160 0
#> 4            3     caudalmiddlefrontal 100  25   0 0
#> 5            4          corpuscallosum 120  70  50 0
#> 6            5                  cuneus 220  20 100 0
```
