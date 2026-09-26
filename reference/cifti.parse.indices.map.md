# Parse one MatrixIndicesMap element.

Parse one MatrixIndicesMap element.

## Usage

``` r
cifti.parse.indices.map(node, dim_sizes, filepath = "")
```

## Arguments

- node:

  an xml2 node, the `MatrixIndicesMap` element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

a named list, see
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).
