# Compute the size of a NIFTI v2 header extension in a file.

A NIFTI v2 header extension occupies a multiple of 16 bytes in the file,
because the payload is padded with NUL bytes. See
[`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md)
for details.

## Usage

``` r
nifti2.extension.size(extension)
```

## Arguments

- extension:

  a NIFTI v2 header extension, a named list with entries 'ecode' and
  'content', see
  [`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).

## Value

integer, the size of the extension in bytes, as it is stored in the
file. This includes the 8 bytes of meta data (the size and code fields)
and the padding.

## See also

Other nifti2 extensions:
[`nifti2.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md),
[`nifti2.extension.content()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.content.md),
[`nifti2.extension.text()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md),
[`nifti2.get.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)
