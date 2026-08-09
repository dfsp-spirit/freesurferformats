# Read colortable file in FreeSurfer ASCII LUT format.

Read a colortable from a text file in FreeSurfer ASCII colortable lookup
table (LUT) format. An example file is
`FREESURFER_HOME/FreeSurferColorLUT.txt`.

## Usage

``` r
read.fs.colortable(filepath, compute_colorcode = FALSE)
```

## Arguments

- filepath, :

  string. Full path to the output colormap file.

- compute_colorcode:

  logical, indicates whether the unique color codes should be computed
  and added to the returned data.frame as an extra integer column named
  'code'. Defaults to FALSE.

## Value

the data.frame that was read from the LUT file. It contains the
following columns that were read from the file: 'struct_index': integer,
index of the struct entry. 'struct_name': character string, the label
name. 'r': integer in range 0-255, the RGBA color value for the red
channel. 'g': same for green channel. 'b': same for blue channel. 'a':
same for alpha (transparency) channel. If 'compute_colorcode' is TRUE,
it also contains the following columns which were computed from the
color values: 'code': integer, unique color identifier computed from the
RGBA values.

## See also

Other atlas functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

Other colorLUT functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

## Examples

``` r
lutfile <- system.file("extdata", "colorlut.txt", package = "freesurferformats", mustWork = TRUE)
colortable <- read.fs.colortable(lutfile, compute_colorcode = TRUE)
head(colortable)
#>   struct_index                struct_name   r   g   b a     code
#> 1            0                    Unknown   0   0   0 0        0
#> 2            1     Left-Cerebral-Exterior  70 130 180 0 11829830
#> 3            2 Left-Cerebral-White-Matter 245 245 245 0 16119285
#> 4            3       Left-Cerebral-Cortex 205  62  78 0  5127885
#> 5            4     Left-Lateral-Ventricle 120  18 134 0  8786552
#> 6            5          Left-Inf-Lat-Vent 196  58 250 0 16399044
```
