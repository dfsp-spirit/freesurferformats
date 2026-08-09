# Create NIFTI v2 header suitable for given data.

Create NIFTI v2 header suitable for given data.

## Usage

``` r
ni2header.for.data(niidata)
```

## Arguments

- niidata:

  array of numeric (integer or double) data, can have up to 7
  dimensions.

## Value

a NIFTI v2 header (see
[`ni2header.template`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.template.md))
in which the datatype, bitpix, dim and dim_raw fields have been set to
values suitable for the given data. Feel free to change the other
fields.
