# Read ANALYZE 7.5 data from file.

Read ANALYZE 7.5 data from file.

## Usage

``` r
read.analyze.data(filepath, header = NULL, drop_empty_dims = TRUE)
```

## Arguments

- filepath:

  character string, the path to the `.hdr` file. The base name without
  the extension is accepted as well, see
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md).

- header:

  optional ANALYZE 7.5 header as returned by
  [`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).
  It will be read from the file automatically if left at `NULL`.

- drop_empty_dims:

  logical, whether to drop empty dimensions in the returned data array,
  see [`drop`](https://rdrr.io/r/base/drop.html).

## Value

the data in the `.img` file of the pair, as an array (or a vector for 1D
data). Note that the fields of the header are not applied in any way:
the data are returned exactly as they are stored in the file, in the
storage order of the format (the first dimension varies fastest, which
is also the order used by R and by the MGH/MGZ format), and the
`funused1` field that SPM uses as a scale factor is not applied (see
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md)
if you want that).

## See also

[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md)

## Examples

``` r
hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
  package = "freesurferformats", mustWork = TRUE
)
data <- read.analyze.data(hdrfile)
dim(data)
#> [1] 4 3 2
```
