# Parse a CIFTI LabelTable element.

Parse a CIFTI LabelTable element.

## Usage

``` r
cifti.parse.label.table(node)
```

## Arguments

- node:

  an xml2 node, the `LabelTable` element, or an `xml_missing` node.

## Value

a data.frame with the columns 'key', 'red', 'green', 'blue', 'alpha',
'label', 'x', 'y' and 'z', or `NULL` if the node is missing.
