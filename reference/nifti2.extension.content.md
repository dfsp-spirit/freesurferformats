# Get the payload of a NIFTI v2 header extension as raw bytes.

Get the payload of a NIFTI v2 header extension as raw bytes.

## Usage

``` r
nifti2.extension.content(extension, strip_nul = TRUE)
```

## Arguments

- extension:

  a NIFTI v2 header extension, a named list with entries 'ecode' and
  'content', see
  [`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).

- strip_nul:

  logical, whether to remove NUL bytes from the payload. This is
  required to convert the payload to a character string.

## Value

a raw vector, the payload of the extension.

## See also

Other nifti2 extensions:
[`nifti2.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md),
[`nifti2.extension.size()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.size.md),
[`nifti2.extension.text()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md),
[`nifti2.get.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)
