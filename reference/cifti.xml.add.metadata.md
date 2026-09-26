# Add a MetaData element to an XML node.

Add a MetaData element to an XML node.

## Usage

``` r
cifti.xml.add.metadata(node, metadata)
```

## Arguments

- node:

  an xml2 node, the parent element.

- metadata:

  named character vector, named list or `NULL`, the name/value pairs.

## Value

`NULL`, invisibly. The element is added as a child of `node`.
