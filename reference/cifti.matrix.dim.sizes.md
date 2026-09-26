# Determine the sizes of the CIFTI matrix dimensions.

The CIFTI-2 matrix dimensions 0, 1, ... are stored in the `dim` field of
the NIFTI-2 header, starting at its 6th entry (R index vectors are
1-based, so matrix dimension 0 is entry 6). The dimension order in the
XML refers to matrix dimensions, so a `MatrixIndicesMap` with
`AppliesToMatrixDimension="0"` describes the 6th entry of `dim`.

## Usage

``` r
cifti.matrix.dim.sizes(niiheader, filepath = "")
```

## Arguments

- niiheader:

  the NIFTI-2 header of the file, required to determine the sizes of the
  matrix dimensions.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

integer vector, the sizes of the matrix dimensions.
