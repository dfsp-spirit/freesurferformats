# Parse a required or optional child element holding integer values.

Parse a required or optional child element holding integer values.

## Usage

``` r
cifti.parse.child.int.vector(node, child_name, filepath = "")
```

## Arguments

- node:

  an xml2 node, the parent element.

- child_name:

  character string, the name of the child element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

integer vector, or `NULL` if the child element is missing or empty.
