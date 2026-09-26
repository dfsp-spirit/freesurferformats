# Parse the series attributes of a MatrixIndicesMap element.

Parse the series attributes of a MatrixIndicesMap element.

## Usage

``` r
cifti.parse.series(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a named list with the entries 'number_of_series_points', 'start',
'step', 'exponent' and 'unit', or `NULL` if the element has no series
attributes.
