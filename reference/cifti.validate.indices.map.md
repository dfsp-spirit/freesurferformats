# Validate a parsed CIFTI MatrixIndicesMap.

Check that the mapping is consistent with the size of the matrix
dimension it describes, and that the index ranges it declares are valid.
These checks exist because a mismatch would silently misalign the data.

## Usage

``` r
cifti.validate.indices.map(map, filepath = "")
```

## Arguments

- map:

  a parsed indices map, see
  [`cifti.parse.indices.map`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parse.indices.map.md).

- filepath:

  character string, the file path, used in error messages.

## Value

`NULL`, invisibly. Stops with a descriptive error if the mapping is
inconsistent.
