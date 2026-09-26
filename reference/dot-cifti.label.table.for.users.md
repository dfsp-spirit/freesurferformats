# Convert a label table to the format the package has always returned.

The readers document a label table with the column names 'Key', 'Red',
'Green', 'Blue', 'Alpha' and 'Label'; keep that spelling (and the column
order) for the user-facing functions, while the internal representation
uses lower case column names and also stores the optional label
coordinates.

## Usage

``` r
.cifti.label.table.for.users(labels)
```

## Arguments

- labels:

  a data.frame as returned by
  [`cifti.label.table`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md).

## Value

a data.frame with the columns 'Key', 'Red', 'Green', 'Blue', 'Alpha' and
'Label'.
