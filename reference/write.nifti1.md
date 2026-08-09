# Write header and data to a file in NIFTI v1 format.

Write header and data to a file in NIFTI v1 format.

## Usage

``` r
write.nifti1(filepath, niidata, niiheader = NULL, ...)
```

## Arguments

- filepath:

  the file to write. The extension should be '.nii' or '.nii.gz'.

- niidata:

  array of numeric or integer data, with up to 7 dimensions. Will be
  written to the file with the datatype and bitpix specified in the
  'niiheader' argument. Set to `NULL` and pass a 'niiheader' to write
  only the header, and remember to adapt 'magic' in the header.

- niiheader:

  an optional NIFTI v1 header that is suitable for the passed 'niidata'.
  If not given, one will be generated with
  [`ni1header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.for.data.md).

- ...:

  additional parameters passed to
  [`ni1header.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.for.data.md).
  Only used if 'niiheader' is `NULL`.

## See also

Other nifti1 writers:
[`nii1header.for.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/nii1header.for.mgh.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md)
