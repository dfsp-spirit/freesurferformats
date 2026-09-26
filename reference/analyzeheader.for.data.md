# Create ANALYZE 7.5 header suitable for given data.

Create ANALYZE 7.5 header suitable for given data.

## Usage

``` r
analyzeheader.for.data(analyzedata, ...)
```

## Arguments

- analyzedata:

  array of numeric (integer or double) data, can have up to 7
  dimensions.

- ...:

  extra parameters passed on to
  [`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md),
  ignored in this function.

## Value

an ANALYZE 7.5 header (see
[`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md))
in which the `datatype`, `bitpix`, `dim`, `cal_min` and `cal_max` fields
have been set to values suitable for the given data. Feel free to change
the other fields, e.g. the voxel sizes in `pix_dim` or the `orient`
code.

## Note

ANALYZE 7.5 has much fewer data types than NIFTI: integers are stored as
32 bit signed integers and floating point data as 32 bit floats, which
is what this function selects (the same choice that
[`ni1header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.for.data.md)
makes for NIFTI v1). To store 16 bit integers, which is the classic
ANALYZE data type, set `datatype` to `4L` and `bitpix` to `16L` in the
returned header after calling this function.

## Examples

``` r
analyzeheader <- analyzeheader.for.data(array(1:24, dim = c(4, 3, 2)))
analyzeheader$datatype
#> [1] 8
```
