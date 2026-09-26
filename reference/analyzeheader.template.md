# Create a template ANALYZE 7.5 header. You will have to adapt it for your use case.

This function returns a valid ANALYZE 7.5 header with all fields
present, filled with the default values of the format. You will most
likely have to adapt at least the `dim`, `datatype`, `bitpix` and
`pix_dim` fields to your data, which is what
[`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md)
does for you.

## Usage

``` r
analyzeheader.template()
```

## Value

named list, the ANALYZE 7.5 header. The fields `endian`, `magic`,
`header_format` and `spm_origin` are not part of the ANALYZE 7.5 header:
`endian` and `magic` describe how the file is stored, `header_format`
tells which variant of the 348 byte header this is ('analyze' or
'nifti1_pair'), and `spm_origin` is the interpretation of the
`originator` field that the SPM software uses.

## Note

The fields `spm_origin` and `magic` are derived when a file is read,
they are documented in
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).

## See also

[`analyzeheader.for.data`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.for.data.md),
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md),
[`write.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/write.analyze.md)

## Examples

``` r
analyzeheader <- analyzeheader.template()
analyzeheader$dim <- c(3L, 4L, 3L, 2L, 1L, 1L, 1L, 1L)
analyzeheader$pix_dim <- c(1., 1., 1., 1., 0., 0., 0., 0.)
```
