# Read file in FreeSurfer annotation format

Read a data annotation file in FreeSurfer format. Such a file assigns a
label and a color to each vertex of a brain surface. The assignment of
labels to vertices is based on at atlas or brain parcellation file.
Typically the atlas is available for some standard template subject, and
the labels are assigned to another subject by registering it to the
template. For a subject (MRI image pre-processed with FreeSurfer) named
'bert', an example file would be 'bert/label/lh.aparc.annot', which
contains the annotation based on the Desikan-Killiany Atlas for the left
hemisphere of bert.

## Usage

``` r
read.fs.annot(
  filepath,
  empty_label_name = "empty",
  metadata = list(),
  default_label_name = ""
)
```

## Arguments

- filepath:

  string. Full path to the input annotation file. Note: gzipped files
  are supported and gz format is assumed if the filepath ends with
  ".gz".

- empty_label_name:

  character string, a base name to use to rename regions with empty name
  in the label table. This should not occur, and you can ignore this
  parameter setting. A warning will be thrown if this ever triggers. Not
  to be confused with parameter `default_label_name`, see below.

- metadata:

  named list of arbitrary metadata to store in the instance.

- default_label_name:

  character string, the label name to use for vertices which have a
  label code that does not occur in the label table. This is typically
  the case for the 'unknown' region, which often has code `0`. You can
  set this to avoid empty region label names. The typical setting would
  be 'unknown', however by default we leave the names as-is, so that
  annots which are read and then written back to files with this library
  do not differ.

## Value

named list, entries are: "vertices" vector of n vertex indices, starting
with 0. "label_codes": vector of n integers, each entry is a color code,
i.e., a value from the 5th column in the table structure included in the
"colortable" entry (see below). "label_names": the n brain structure
names for the vertices, already retrieved from the colortable using the
code. "hex_colors_rgb": Vector of hex color for each vertex. The
"colortable" is another named list with 3 entries: "num_entries": int,
number of brain structures. "struct_names": vector of strings, the brain
structure names. "table": numeric matrix with num_entries rows and 5
colums. The 5 columns are: 1 = color red channel, 2=color blue channel,
3=color green channel, 4=color alpha channel, 5=unique color code.
"colortable_df": The same information as a dataframe. Contains the extra
columns "hex_color_string_rgb" and "hex_color_string_rgba" that hold the
color as an RGB(A) hex string, like "#rrggbbaa".

## See also

Other atlas functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md),
[`write.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.colortable.md)

## Examples

``` r
annot_file <- system.file("extdata", "lh.aparc.annot.gz",
  package = "freesurferformats",
  mustWork = TRUE
)
annot <- read.fs.annot(annot_file)
print(annot)
#> Brain surface annotation assigning 149244 vertices to 36 brain regions.
#>  - region #0 'unknown': size 0 vertices
#>  - region #1 'bankssts': size 1722 vertices
#>  - region #2 'caudalanteriorcingulate': size 1067 vertices
#>  - region #3 'caudalmiddlefrontal': size 4179 vertices
#>  - region #4 'corpuscallosum': size 0 vertices
#>  - region #5 'cuneus': size 2183 vertices
#>  - region #6 'entorhinal': size 718 vertices
#>  - region #7 'fusiform': size 4773 vertices
#>  - region #8 'inferiorparietal': size 7930 vertices
#>  - region #9 'inferiortemporal': size 5138 vertices
#>  - region #10 'isthmuscingulate': size 1479 vertices
#>  - region #11 'lateraloccipital': size 8827 vertices
#>  - region #12 'lateralorbitofrontal': size 4426 vertices
#>  - region #13 'lingual': size 3693 vertices
#>  - region #14 'medialorbitofrontal': size 3484 vertices
#>  - region #15 'middletemporal': size 5546 vertices
#>  - region #16 'parahippocampal': size 1254 vertices
#>  - region #17 'paracentral': size 2293 vertices
#>  - region #18 'parsopercularis': size 2444 vertices
#>  - region #19 'parsorbitalis': size 1145 vertices
#>  - region #20 'parstriangularis': size 2089 vertices
#>  - region #21 'pericalcarine': size 1830 vertices
#>  - region #22 'postcentral': size 7514 vertices
#>  - region #23 'posteriorcingulate': size 1766 vertices
#>  - region #24 'precentral': size 8410 vertices
#>  - region #25 'precuneus': size 5987 vertices
#>  - region #26 'rostralanteriorcingulate': size 1204 vertices
#>  - region #27 'rostralmiddlefrontal': size 9447 vertices
#>  - region #28 'superiorfrontal': size 12569 vertices
#>  - region #29 'superiorparietal': size 8110 vertices
#>  - region #30 'superiortemporal': size 6987 vertices
#>  - region #31 'supramarginal': size 6656 vertices
#>  - region #32 'frontalpole': size 416 vertices
#>  - region #33 'temporalpole': size 637 vertices
#>  - region #34 'transversetemporal': size 828 vertices
#>  - region #35 'insula': size 4099 vertices
```
