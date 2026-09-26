# Read ANALYZE 7.5 header from file.

Read the header of an ANALYZE 7.5 file, i.e. of a file in the two-file
format that stores the voxel data in a separate `.img` file.

## Usage

``` r
read.analyze.header(filepath)
```

## Arguments

- filepath:

  character string, the path to the `.hdr` file. The base name without
  the extension is accepted as well, see
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md).

## Value

named list with the ANALYZE 7.5 header fields, in the field naming of
the format specification. See
[`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md)
for a description of all of them. In addition to the fields of the
specification, the following entries are present:

- `endian`:

  character string, the endianness of the file, `'little'` or `'big'`.
  It is detected from the `sizeof_hdr` field.

- `magic`:

  character string, the 4 bytes at offset 344 (which are the `smin`
  field of the ANALYZE specification), interpreted as a string. It is
  empty for ANALYZE files, see
  [`is.analyze.file`](https://dfsp-spirit.github.io/freesurferformats/reference/is.analyze.file.md).

- `header_format`:

  character string, always `'analyze'` here. The other variant of the
  two-file format is reported as `'nifti1_pair'`, see
  [`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md).

- `spm_origin`:

  integer vector of length 3, the interpretation that SPM gives to the
  `originator` field: the voxel coordinates of the image origin, as
  stored in the first 6 bytes of the field. It is `NULL` if all of them
  are zero, which means that the field does not store an origin.

- `originator_bytes`:

  raw vector of length 10, the unmodified content of the `originator`
  field. The `originator` entry is the same content with the zero bytes
  removed and interpreted as a string, which loses information for the
  files in which SPM stores the image origin there.

- `filepath_header`, `filepath_image`:

  character strings, the paths of the two files of the pair.

## Note

ANALYZE 7.5 does not define the meaning of the world coordinate system
of an image: the header stores the voxel sizes but neither the direction
of the voxel axes nor the position of the image in space. This function
therefore reports the fields as they are stored and does not derive a
transformation matrix. See
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md)
for the ways to get a matrix anyway, and for what is known and unknown
about them.

## See also

[`read.analyze.data`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.data.md),
[`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md)

## Examples

``` r
hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
  package = "freesurferformats", mustWork = TRUE
)
analyzeheader <- read.analyze.header(hdrfile)
analyzeheader$datatype
#> [1] 2
analyzeheader$pix_dim
#> [1] 1 1 2 3 1 1 1 1
```
