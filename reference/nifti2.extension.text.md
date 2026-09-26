# Get the payload of a NIFTI v2 header extension as text.

Convert the payload of a header extension to a character string. The
trailing NUL bytes, which are only used to pad the extension to a
multiple of 16 bytes, are removed. Note that embedded NUL bytes cannot
be represented in an R string and are removed as well, so this function
is only meaningful for text payloads (like the CIFTI2 XML metadata).

## Usage

``` r
nifti2.extension.text(extension)
```

## Arguments

- extension:

  a NIFTI v2 header extension, a named list with entries 'ecode' and
  'content', see
  [`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).

## Value

character string, the payload of the extension. The string is marked as
UTF-8 encoded, which is the encoding required by the NIFTI standard.

## See also

Other nifti2 extensions:
[`nifti2.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md),
[`nifti2.extension.content()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.content.md),
[`nifti2.extension.size()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.size.md),
[`nifti2.get.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)
