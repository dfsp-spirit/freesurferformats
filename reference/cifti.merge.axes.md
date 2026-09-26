# Merge explicitly given axes with the axes of a template.

Merge explicitly given axes with the axes of a template.

## Usage

``` r
cifti.merge.axes(axes, template_cii)
```

## Arguments

- axes:

  list of axes or `NULL`, see
  [`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md).

- template_cii:

  an `fs.cifti` object or `NULL`.

## Value

the merged list of axes, named by matrix dimension.
