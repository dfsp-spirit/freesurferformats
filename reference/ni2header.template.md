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

The 'magic' field of a NIFTI v2 file must be the string 'n+2'. Versions
of this package before 1.1.0 wrote the NIFTI v1 magic 'n+1' here, which
violates the standard and makes other software (nibabel, Connectome
Workbench) refuse the file;
[`write.nifti2`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti2.md)
writes the full 8 byte magic of the standard.

## See also

[`ni2header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.for.data.md)
