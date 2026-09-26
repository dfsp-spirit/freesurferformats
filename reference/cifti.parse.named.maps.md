# Parse the NamedMap elements of a MatrixIndicesMap element.

Parse the NamedMap elements of a MatrixIndicesMap element.

## Usage

``` r
cifti.parse.named.maps(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a list of lists with the entries 'name', 'metadata' and 'labels', or
`NULL` if there are no NamedMap elements.
