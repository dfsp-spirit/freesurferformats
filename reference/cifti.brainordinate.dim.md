# Get the brainordinate dimension of a CIFTI-2 file.

Find the matrix dimension of a CIFTI-2 file that holds the brain
structures. Most files have exactly one (dimension 1 for a `.dscalar`,
`.dtseries` or `.dlabel`, dimension 0 for the special case of a
`.dpconn`-style file), while connectome files like a `.dconn` have brain
models in both dimensions.

## Usage

``` r
cifti.brainordinate.dim(cii)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

## Value

integer, the dimension. Stops with a descriptive error if there is no
brainordinate dimension or if there are two.
