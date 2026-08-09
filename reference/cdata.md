# Create CDATA element string from string.

Create CDATA element string from string.

## Usage

``` r
cdata(string)
```

## Arguments

- string:

  character string, the input string, freeform text. Must not contain
  the cdata start and end tags.

## Value

character string, the input wrapped in the cdata tags

## Note

This returns a string, not an XML node. See
[`xml_cdata`](http://xml2.r-lib.org/reference/xml_cdata.md) if you want
a node.
