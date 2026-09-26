# Write header and data to a file in NIFTI v1 format.

Write header and data to a file in NIFTI v1 format.

## Usage

``` r
write.nifti1(filepath, niidata, niiheader = NULL, ...)
```

## Arguments

- filepath:

  the file to write. The extension should be '.nii' or '.nii.gz' for a
  single file NIFTI image, and '.hdr', '.img' (optionally followed by
  '.gz') for a NIFTI v1 pair, i.e. a header file with the voxel data in
  a separate '.img' file. Which of the two is written follows from the
  `magic` field of the header: 'n+1' means a single file, 'ni1' means a
  pair. See
  [`ni1header.template`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.template.md).

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

## Value

invisible named list with the entries `header` (the header that was
written) and `data` (the data that was written, or `NULL` if no data was
passed).

## See also

Other nifti1 writers:
[`nii1header.for.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/nii1header.for.mgh.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md)

## Examples

``` r
# A single file NIFTI image (the default):
data <- array(1:24, dim = c(4, 3, 2))
nii_file <- tempfile(fileext = ".nii")
write.nifti1(nii_file, data)

# A NIFTI v1 pair, i.e. a header plus a separate data file. Ask for a pair
# header by passing 'pair = TRUE' (this is passed on to the header generator):
write.nifti1(tempfile(fileext = ".hdr"), data, pair = TRUE)

# The NIFTI v1 pair files can be read back with the volume reader:
hdr_file <- tempfile(fileext = ".hdr")
write.nifti1(hdr_file, data, pair = TRUE)
vol <- read.fs.volume(hdr_file, with_header = TRUE)
print(dim(vol$data))
#> [1] 4 3 2
```
