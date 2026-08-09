# Create XML GIFTI metadata node.

Create XML GIFTI metadata node.

## Usage

``` r
xml_node_gifti_MD(name, value, as_cdata = TRUE)
```

## Arguments

- name:

  character string, the metadata name

- value:

  character string, the metadata value

- as_cdata:

  logical, whether to wrap the value in cdata tags

## Value

XML tree from xml2

## Note

This creates an MD note, not the outer MetaData node.
