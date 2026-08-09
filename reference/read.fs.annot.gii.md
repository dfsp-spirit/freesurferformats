# Read an annotation or label in GIFTI format.

Read an annotation or label in GIFTI format.

## Usage

``` r
read.fs.annot.gii(
  filepath,
  element_index = 1L,
  labels_only = FALSE,
  rgb_column_names = c("Red", "Green", "Blue", "Alpha"),
  key_column_name = "Key",
  empty_label_name = "unknown"
)
```

## Arguments

- filepath:

  string. Full path to the input label file in GIFTI format.

- element_index:

  positive integer, the index of the dataarray to return. Ignored unless
  the file contains several dataarrays.

- labels_only:

  logical, whether to ignore the colortable and region names. The
  returned annotation will only contain the a vector that contains one
  integer label per vertex (as entry 'label_codes'), but no region names
  and colortable information.

- rgb_column_names:

  vector of exactly 4 character strings, order is important. The column
  names for the red, green, blue and alpha channels in the lable table.
  If a column does not exist, pass NA. If you do not know the column
  names, just call the function, it will print them. See 'labels_only'
  if you do not care.

- key_column_name:

  character string, the column name for the key column in the lable
  table. This is the column that holds the label value from the raw
  vector (see 'labels_only') that links a label value to a row in the
  label table. Without it, one cannot recostruct the region name and
  color of an entry. Passing NA has the same effect as setting
  'labels_only' to TRUE.

- empty_label_name:

  character string, a base name to use to rename regions with empty name
  in the label table. This should not occur, and you can ignore this
  parameter setting. A warning will be thrown if this ever triggers. Not
  to be confused with parameter `default_label_name`, see below.

## See also

Other gifti readers:
[`read.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.gii.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
[`read.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.gii.md)
