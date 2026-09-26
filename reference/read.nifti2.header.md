# Read NIFTI v2 header from file.

Read NIFTI v2 header from file.

## Usage

``` r
read.nifti2.header(filepath)
```

## Arguments

- filepath:

  path to a NIFTI v2 file.

## Value

named list with NIFTI 2 header fields. The header extensions, if the
file has any, are returned in the field `extensions`, a list in which
each entry is one header extension as created by
[`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md)
(i.e., a list with the entries 'ecode' and 'content'). CIFTI2 files
store their XML metadata in such an extension, see
[`nifti2.get.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md).

## See also

[`write.nifti2`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti2.md)
