# Create NIFTI v1 header suitable for given data.

Create NIFTI v1 header suitable for given data.

## Usage

``` r
ni1header.for.data(niidata, allow_fshack = FALSE)
```

## Arguments

- niidata:

  array of numeric (integer or double) data, can have up to 7
  dimensions.

- allow_fshack:

  logical, whether to allow data in which the first dimension is larger
  than 32767, and use the FreeSurfer NIFTI v1 hack to support his. The
  hack will be used only if needed. WARNING: Files written with the hack
  do not conform to the NIFTI v1 standard and will not be read correctly
  by most software. All FreeSurfer tools and the Python 'nibabel' module
  support it.

## Value

a NIFTI v1 header (see
[`ni1header.template`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.template.md))
in which the datatype, bitpix, dim and dim_raw fields have been set to
values suitable for the given data. Feel free to change the other
fields.
