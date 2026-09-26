# Parse the Volume elements of a MatrixIndicesMap element.

Parse the Volume elements of a MatrixIndicesMap element.

## Usage

``` r
cifti.parse.volumes(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a list of lists with the entries 'dimensions', 'meter_exponent' and
'transformation_matrix', or `NULL` if there are no Volume elements.
