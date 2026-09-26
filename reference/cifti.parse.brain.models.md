# Parse the BrainModel elements of a MatrixIndicesMap element.

Parse the BrainModel elements of a MatrixIndicesMap element.

## Usage

``` r
cifti.parse.brain.models(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a list of lists, or `NULL` if there are no BrainModel elements.
