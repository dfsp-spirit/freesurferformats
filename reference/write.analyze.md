# Write data to a file in ANALYZE 7.5 format.

Write an array or vector to the two files that make up an ANALYZE 7.5
image: a 348 byte header (`<base>.hdr`) and the raw voxel data in a
separate file (`<base>.img`).

## Usage

``` r
write.analyze(filepath, analyzedata, analyzeheader = NULL, ...)
```

## Arguments

- filepath:

  character string, the path of the output files. It must end with
  `.hdr` or `.img` (optionally followed by `.gz`), or contain no
  extension at all, in which case the two file names are derived from it
  by appending `.hdr` and `.img`. See
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md).

- analyzedata:

  array of numeric (integer or double) data, can have up to 7
  dimensions. See
  [`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md)
  for how the data type is chosen, and note that the data type and the
  number of bits per value in the header have to match the data,
  otherwise the file is not readable.

- analyzeheader:

  optional ANALYZE 7.5 header that is suitable for the passed
  `analyzedata`, see
  [`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md)
  and
  [`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md).
  If not given, one is generated from the data, with all other fields
  set to the defaults of the format, i.e. the voxel sizes in `pix_dim`
  will be zero and you most likely want to set them.

- ...:

  additional parameters passed to
  [`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md).
  Only used if `analyzeheader` is `NULL`.

## Value

invisible named list with the entries `header` (the header that was
written) and `data` (the data that was written). Use these to check what
the function did with your input, they reflect the values that ended up
in the file, e.g. values that had to be converted to the data type
stated in the header.

## Note

ANALYZE 7.5 cannot store a transformation matrix, and the format does
not even define the direction of the voxel axes in world space. This
writer therefore cannot write the geometry of an image: if you have a
volume with a known `vox2ras_matrix` and you want to keep it, write a
NIFTI v1 file with
[`write.nifti1`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti1.md)
instead (either a single file, or a `.hdr`/`.img` pair by setting the
header magic to `'ni1'`, which is what
[`write.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.volume.md)
does for such file names). If you know that the data will be read by SPM
or FreeSurfer, you can store the image origin in the `spm_origin` field
of the header, see
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).

Note that tools differ in how they recognize the format. FreeSurfer's
`mri_convert`, for example, does not infer the ANALYZE format from the
`.hdr` file extension and reports 'cannot determine file type' unless
the input type is given explicitly:
`mri_convert -it analyze -ot mgz vol.hdr vol.mgz` works,
`mri_convert -ot mgz vol.hdr vol.mgz` does not. The header written by
this function is the one FreeSurfer itself writes for `-ot analyze`, so
this is a limitation of the reader, not of the file.

## See also

[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md),
[`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md)

## Examples

``` r
outfiles <- tempfile()
data <- array(1:24, dim = c(4, 3, 2))
analyzeheader <- analyzeheader.for.data(data)
analyzeheader$pix_dim <- c(1., 1.5, 1.5, 2., 0., 0., 0., 0.)
write.analyze(outfiles, data, analyzeheader)
vol <- read.fs.volume.analyze(outfiles, with_header = TRUE)
vol$header$pix_dim
#> [1] 1.0 1.5 1.5 2.0 0.0 0.0 0.0 0.0
```
