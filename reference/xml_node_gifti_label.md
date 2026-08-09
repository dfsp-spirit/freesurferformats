# Create XML GIFTI Label node.

Create XML GIFTI Label node.

## Usage

``` r
xml_node_gifti_label(value, attributes = list(), as_cdata = TRUE)
```

## Arguments

- value:

  the text contents of the node, usually the region name

- attributes:

  named list, the attributes

- as_cdata:

  logical, whether to wrap the value in cdata tags

## Value

XML node from xml2
