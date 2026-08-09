# Write XML tree to a gifti file.

Write XML tree to a gifti file.

## Usage

``` r
gifti_xml_write(filepath, xmltree, options = c("as_xml", "format"))
```

## Arguments

- filepath:

  path to the output gifti file

- xmltree:

  XML tree from xml2

- options:

  output options passed to
  [`write_xml`](http://xml2.r-lib.org/reference/write_xml.md).

## References

<https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf>

## Examples

``` r
if (FALSE) { # \dontrun{
outfile <- tempfile(fileext = ".gii")
my_data_sets <- list(rep(3.1, 3L), matrix(seq(6) + 0.1, nrow = 2L))
xmltree <- gifti_xml(my_data_sets, datatype = "NIFTI_TYPE_FLOAT32")
# Here we add global metadata:
xmltree <- gifti_xml_add_global_metadata(xmltree, list("User" = "Me", "Day" = "Monday"))
# Validating your XML never hurts
gifti_xsd <- "https://www.nitrc.org/frs/download.php/158/gifti.xsd"
xml2::xml_validate(xmltree, xml2::read_xml(gifti_xsd))
gifti_xml_write(outfile, xmltree)
# Write your custom tree to a file.
} # }
```
