# Check whether a file is an ANALYZE 7.5 file.

Check whether a file is an ANALYZE 7.5 file.

## Usage

``` r
is.analyze.file(filepath)
```

## Arguments

- filepath:

  character string, the path to a `.hdr` file (or to a file whose name
  ends with the base name of one, see
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md)).

## Value

logical, whether the 348 byte header is an ANALYZE 7.5 header. This is
the case when the header contains neither of the two NIFTI v1 magic
strings, i.e. when the magic is empty. Files that use the `ni1` magic
are NIFTI v1 pair files, see
[`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md),
and files with the `n+1` magic are single file NIFTI v1 files.

## Note

Note that this function returns `TRUE` for any two-file header that does
not carry a NIFTI magic, which is what "ANALYZE 7.5" means in practice:
these files were written by ANALYZE itself, by SPM, by AFNI or by
FreeSurfer. Only the fields of the ANALYZE specification are defined for
them, but SPM stores extra information in fields that ANALYZE leaves
unused, see
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md).

A file path that does not describe an existing pair file (for example a
single file NIFTI image, or a file that does not exist) returns `FALSE`
rather than an error.

## Examples

``` r
hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
  package = "freesurferformats", mustWork = TRUE
)
is.analyze.file(hdrfile)
#> [1] TRUE
```
