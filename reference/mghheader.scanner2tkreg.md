# Compute scanner-RAS 2 tkreg-RAS matrix from basic MGH header fields.

This is also known as the 'scanner2tkreg' matrix. Note that this is a
RAS-to-RAS matrix. It is the inverse of the 'tkreg2scanner' matrix, see
[`mghheader.tkreg2scanner`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md).

## Usage

``` r
mghheader.scanner2tkreg(header)
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
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
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
mghheader.scanner2tkreg(vdh$header)
#>      [,1] [,2] [,3]        [,4]
#> [1,]    1    0    0   0.4999542
#> [2,]    0    1    0 -29.3727417
#> [3,]    0    0    1  48.9047318
#> [4,]    0    0    0   1.0000000
```
