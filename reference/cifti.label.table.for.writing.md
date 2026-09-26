# Convert a label table to the format the writer expects.

Convert a label table to the format the writer expects.

## Usage

``` r
cifti.label.table.for.writing(label_table)
```

## Arguments

- label_table:

  a data.frame, see
  [`cifti.axis.labels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.labels.md),
  the colortable of an `fs.annot` instance, or `NULL`. The column names
  are matched case-insensitively, so the label table that
  [`read.fs.parcellation.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.parcellation.cifti.md)
  returns (with the columns 'Key', 'Red', 'Green', 'Blue', 'Alpha' and
  'Label') can be passed directly. The colortable of a FreeSurfer
  annotation stores the colors in the range 0 to 255, and it is
  converted here.

## Value

a data.frame with the columns 'key', 'red', 'green', 'blue', 'alpha' and
'label', or `NULL`.
