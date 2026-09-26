# Accept the input forms of a CIFTI-2 connectome.

Accept the input forms of a CIFTI-2 connectome.

## Usage

``` r
cifti.data.for.connectome(filepath, rows = NULL, columns = NULL)
```

## Arguments

- filepath:

  character string, `fs.cifti` or `fs.cifti.data`, see
  [`read.fs.connectome.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md).

- rows:

  integer vector or `NULL`, the indices of matrix dimension 0 to read.

- columns:

  integer vector or `NULL`, the indices of matrix dimension 1 to read.

## Value

an `fs.cifti.data` object, see
[`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md).
