# Parse the Surface elements of a MatrixIndicesMap element.

Parse the Surface elements of a MatrixIndicesMap element.

## Usage

``` r
cifti.parse.surfaces(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a list of lists with the entries 'brain_structure' and
'surface_number_of_vertices', or `NULL` if there are no Surface
elements.
