# Create a NIFTI v2 header extension.

NIFTI v2 files can store arbitrary data in header extensions, in the
area between the header and the voxel data. This function creates such
an extension. Pass the result to
[`write.nifti2`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti2.md),
which writes it to the file, and later read it back with
[`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)
plus
[`nifti2.get.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md).

## Usage

``` r
nifti2.extension(ecode, content = NULL)
```

## Arguments

- ecode:

  integer, the extension code. This identifies the type of the payload,
  but the code is not interpreted in any way. The CIFTI2 XML metadata
  uses code `32L`.

- content:

  raw vector, character string, or `NULL`. The payload of the extension,
  i.e., the data to store. A character string is converted to UTF-8
  bytes (several strings are joined with newline characters). Use `NULL`
  or omit for an empty payload.

## Value

named list with the entries 'ecode' and 'content', representing one
NIFTI v2 header extension.

## See also

[`nifti2.get.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md),
[`nifti2.extension.text`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md)

Other nifti2 extensions:
[`nifti2.extension.content()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.content.md),
[`nifti2.extension.size()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.size.md),
[`nifti2.extension.text()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.text.md),
[`nifti2.get.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)
