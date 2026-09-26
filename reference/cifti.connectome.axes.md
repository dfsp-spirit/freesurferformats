# Determine the two axes of a connectome file.

Determine the two axes of a connectome file.

## Usage

``` r
cifti.connectome.axes(axes, template_cii = NULL)
```

## Arguments

- axes:

  list of one or two axes or `NULL`, see
  [`write.fs.connectome.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md).

- template_cii:

  an `fs.cifti` object or `NULL`, see
  [`write.fs.connectome.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md).

## Value

a list of two axes, named by matrix dimension.
