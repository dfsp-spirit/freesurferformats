# Format a number like the reference implementations do.

Connectome Workbench writes fixed point values with 10 decimal places in
the XML (e.g. `SeriesStep="2.5000000000"`), and this keeps the output
comparable with the files it writes. Colors of a label table are written
with the precision the file had, which is 3 decimal places in the
official example files.

## Usage

``` r
cifti.xml.num(x, decimals = 10L)
```

## Arguments

- x:

  numeric, the value.

- decimals:

  integer, the number of decimal places.

## Value

character string.
