# Read a label file.

Read a label file.

## Usage

``` r
read.fs.label(filepath, format = "auto", ...)
```

## Arguments

- filepath:

  string. Full path to the input label file.

- format:

  character string, one of 'auto' to detect by file extension, 'asc' for
  native FreeSurfer ASCII label format, or 'gii' for GIFTI label format.

- ...:

  extra paramters passed to the respective label function for the format

## Note

See
[`read.fs.label.native`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.native.md)
for more details, including important information on loading FreeSurfer
volume labels.

## See also

Other label functions:
[`read.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.gii.md),
[`read.fs.label.native()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.native.md),
[`write.fs.label()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.md)

## Examples

``` r
labelfile <- system.file("extdata", "lh.entorhinal_exvivo.label",
  package = "freesurferformats", mustWork = TRUE
)
label <- read.fs.label(labelfile)
```
