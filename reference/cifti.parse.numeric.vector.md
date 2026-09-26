# Parse a whitespace-separated list of numbers from XML text content.

Parse a whitespace-separated list of numbers from XML text content.

## Usage

``` r
cifti.parse.numeric.vector(text, what = "an element", context = "")
```

## Arguments

- text:

  character string, the text content of an XML element.

- what:

  character string, the element/attribute name, used in error messages.

- context:

  character string, additional context for error messages.

## Value

numeric vector. Empty numeric vector for empty input.
