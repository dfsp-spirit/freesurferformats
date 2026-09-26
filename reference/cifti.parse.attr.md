# Read and validate an attribute value of an XML node.

Read and validate an attribute value of an XML node.

## Usage

``` r
cifti.parse.attr(node, attr_name, required = FALSE, context = "")
```

## Arguments

- node:

  an xml2 node.

- attr_name:

  character string, the name of the attribute.

- required:

  logical, whether the attribute must be present.

- context:

  character string, used in the error message.

## Value

character string, or `NA_character_` if the attribute is absent and not
required.
