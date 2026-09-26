# Add a MatrixIndicesMap element to the Matrix element.

Add a MatrixIndicesMap element to the Matrix element.

## Usage

``` r
cifti.xml.add.indices.map(matrix_node, axis, dims)
```

## Arguments

- matrix_node:

  an xml2 node, the `Matrix` element.

- axis:

  the axis to write, see
  [`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md).

- dims:

  integer vector, the matrix dimensions the element applies to.

## Value

`NULL`, invisibly. The element is added as a child of `matrix_node`.
