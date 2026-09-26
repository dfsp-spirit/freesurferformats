# Compute vox2ras matrix from basic MGH header fields.

This is also known as the 'scanner' or 'native' vox2ras. It is the
inverse of the respective ras2vox, see
[`mghheader.ras2vox`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md).

## Usage

``` r
mghheader.vox2ras(header)
```

## Arguments

- header:

  the MGH header

## Value

4x4 numerical matrix, the transformation matrix

## See also

[`sm0to1`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md)

Other header coordinate space:
[`invert.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/invert.fs.transform.md),
[`is.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/is.fs.transform.md),
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
[`print.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.transform.md),
[`read.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.md),
[`read.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md),
[`read.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.fslmat.md),
[`read.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.itk.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md),
[`summary.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/summary.fs.transform.md),
[`transform2lps()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2lps.md),
[`transform2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2ras.md),
[`transform2voxel()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md),
[`transform2world()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2world.md),
[`write.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.md),
[`write.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.dat.md),
[`write.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.fslmat.md),
[`write.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md),
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
brain_image <- system.file("extdata", "brain.mgz",
  package = "freesurferformats",
  mustWork = TRUE
)
vdh <- read.fs.mgh(brain_image, with_header = TRUE)
mghheader.vox2ras(vdh$header)
#>      [,1] [,2] [,3]      [,4]
#> [1,]   -1    0    0 127.50005
#> [2,]    0    0    1 -98.62726
#> [3,]    0   -1    0  79.09527
#> [4,]    0    0    0   1.00000
```
