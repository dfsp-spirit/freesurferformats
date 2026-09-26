# Get a NIFTI v2 header extension by code.

Retrieve one header extension from a NIFTI v2 header, by its extension
code. See
[`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md)
for details on header extensions.

## Usage

``` r
nifti2.get.extension(niiheader, ecode)
```

## Arguments

- niiheader:

  named list, a NIFTI v2 header as returned by
  [`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)
  or created by
  [`ni2header.template`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.template.md).

- ecode:

  integer, the extension code to search for. The CIFTI2 XML metadata
  uses code `32L`.

## Value

the extension (a named list with entries 'ecode' and 'content', see
[`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md)),
or `NULL` if the header contains no extension with this code.

## See also

[`nifti2.extension.text`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md)

Other nifti2 extensions:
[`nifti2.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md),
[`nifti2.extension.content()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.content.md),
[`nifti2.extension.size()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.size.md),
[`nifti2.extension.text()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md)
