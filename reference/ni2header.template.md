# Create a template NIFTI v2 header. You will have to adapt it for your use case.

Create a template NIFTI v2 header. You will have to adapt it for your
use case.

## Usage

``` r
ni2header.template()
```

## Value

named list, the NIFTI v2 header. All fields are present and filled with
values of a proper type. Whether or not they make sense is up to you,
but you will most likely have to adapt at least the following fields to
your data: `dim_raw`, `datatype`, `bitpix`.

## Note

Commonly used data type settings are: for signed integers datatype =
`8L` and bitpix = `32L`; for floats datatype = `16L` and bitpix = `32L`.
See the NIFTI v2 standard for more options. You may want to call
[`ni2header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.for.data.md)
instead of this function.

## See also

[`ni2header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.for.data.md)
