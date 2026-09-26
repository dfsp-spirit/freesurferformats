# Validate the axes of a CIFTI-2 file.

Check the things about a set of axes that make a file invalid rather
than just unusual: the sizes of the axes, the index ranges of the brain
models, the structures a volume model or a parcel refers to, and the
label keys of a label table.

## Usage

``` r
cifti.validate.axes(axes)
```

## Arguments

- axes:

  list of axes, see
  [`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md).

## Value

`NULL`, invisibly. Stops with a descriptive error.
