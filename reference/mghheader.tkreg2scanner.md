# Compute tkreg-RAS to scanner-RAS matrix from basic MGH header fields.

This is also known as the 'tkreg2scanner' matrix. Note that this is a
RAS-to-RAS matrix. It is the inverse of the 'scanner2tkreg' matrix, see
[`mghheader.scanner2tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md).

## Usage

``` r
mghheader.tkreg2scanner(header)
```

## Arguments

- header:

  the MGH header

## Value

4x4 numerical matrix, the transformation matrix

## See also

Other header coordinate space:
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
[`mghheader.vox2ras.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
[`read.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.md),
[`read.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)

## Examples

``` r
brain_image <- system.file("extdata", "brain.mgz",
  package = "freesurferformats",
  mustWork = TRUE
)
vdh <- read.fs.mgh(brain_image, with_header = TRUE)
mghheader.tkreg2scanner(vdh$header)
#>      [,1] [,2] [,3]        [,4]
#> [1,]    1    0    0  -0.4999542
#> [2,]    0    1    0  29.3727417
#> [3,]    0    0    1 -48.9047318
#> [4,]    0    0    0   1.0000000
```
