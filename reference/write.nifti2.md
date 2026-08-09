# Write header and data to a file in NIFTI v2 format.

Write header and data to a file in NIFTI v2 format.

## Usage

``` r
write.nifti2(filepath, niidata, niiheader = NULL)
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

## See also

Other nifti2 writers:
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md)
