# Add metadata to GIFTI XML tree.

Add metadata to GIFTI XML tree.

## Usage

``` r
gifti_xml_add_global_metadata(xmltree, metadata_named_list, as_cdata = TRUE)
```

## Arguments

- xmltree:

  XML tree from xml2

- metadata_named_list:

  named list, the metadata entries

- as_cdata:

  logical, whether to wrap the value in cdata tags

## Value

the modified tree.

## Note

Assumes that there already exists a global MetaData node. Also not that
this is not supposed to be used for adding metadata to datarrays.

## Examples

``` r
if (FALSE) { # \dontrun{
xmltree <- gifti_xml(list(rep(3.1, 3L), matrix(seq(6) + 0.1, nrow = 2L)))
newtree <- gifti_xml_add_global_metadata(xmltree, list("User" = "Me", "Weather" = "Great"))
gifti_xsd <- "https://www.nitrc.org/frs/download.php/158/gifti.xsd"
xml2::xml_validate(newtree, xml2::read_xml(gifti_xsd))
} # }
```
