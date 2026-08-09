# Write colortable file in FreeSurfer ASCII LUT format.

Write the colortable to a text file in FreeSurfer ASCII colortable
lookup table (LUT) format. An example file is
`FREESURFER_HOME/FreeSurferColorLUT.txt`.

## Usage

``` r
write.fs.colortable(filepath, colortable)
```

## Arguments

- filepath, :

  string. Full path to the output colormap file.

- colortable:

  data.frame, a colortable as read by
  [`read.fs.colortable`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md).
  Must contain the following columns: 'struct_name': character string,
  the label name. 'r': integer in range 0-255, the RGBA color value for
  the red channel. 'g': same for green channel. 'b': same for blue
  channel. 'a': same for alpha (transparency) channel. Can contain the
  following column: 'struct_index': integer, index of the struct entry.
  If this column does not exist, sequential indices starting at zero are
  created.

## Value

the written dataframe, invisible. Note that this is will contain a
column named 'struct_index', no matter whether the input colortable
contained it or not.

## See also

Other atlas functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md),
[`write.fs.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.md),
[`write.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.annot.gii.md)

Other colorLUT functions:
[`colortable.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/colortable.from.annot.md),
[`read.fs.colortable()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.colortable.md)
