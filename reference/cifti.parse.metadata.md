# Parse the MetaData element of a CIFTI XML node.

Parse the MetaData element of a CIFTI XML node.

## Usage

``` r
cifti.parse.metadata(node)
```

## Arguments

- node:

  an xml2 node that may contain a `MetaData` child element.

## Value

a named list of character strings, the metadata entries in file order.
Empty list if the node has no metadata.
