# Write header and data to a file in NIFTI v2 format.

Write header and data to a file in NIFTI v2 format.

## Usage

``` r
write.nifti2(filepath, niidata, niiheader = NULL, extensions = NULL)
```

## Arguments

- filepath:

  the file to write. The extension should be '.nii' or '.nii.gz'.

- niidata:

  array of numeric or integer data, with up to 7 dimensions. Will be
  written to the file with the datatype and bitpix specified in the
  'niiheader' argument.

- niiheader:

  an optional NIFTI v2 header that is suitable for the passed 'niidata'.
  If not given, one will be generated with
  [`ni2header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni2header.for.data.md).

- extensions:

  optional list of NIFTI v2 header extensions to write between the
  header and the data, each created with
  [`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).
  If left at `NULL` and the 'niiheader' has a field named 'extensions',
  that field is used, so that a header read with
  [`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)
  can be written back to a new file without losing its extensions. The
  'vox_offset' field of the header is adapted if it is too small to fit
  the extensions, and the data is written directly after them.

## See also

[`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md),
[`nifti2.get.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)

Other nifti2 writers:
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md)
