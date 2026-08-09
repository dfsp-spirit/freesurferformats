# Compute vox2ras-tkreg matrix from basic MGH header fields.

This is also known as the 'tkreg' vox2ras. It is the inverse of the
respective ras2vox, see
[`mghheader.ras2vox.tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md).

## Usage

``` r
mghheader.vox2ras.tkreg(header)
```

## Arguments

- header:

  the MGH header

## Value

4x4 numerical matrix, the transformation matrix

## See also

[`sm0to1`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md)

Other header coordinate space:
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
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
mghheader.vox2ras.tkreg(vdh$header)
#>      [,1] [,2] [,3] [,4]
#> [1,]   -1    0    0  128
#> [2,]    0    0    1 -128
#> [3,]    0   -1    0  128
#> [4,]    0    0    0    1
```
