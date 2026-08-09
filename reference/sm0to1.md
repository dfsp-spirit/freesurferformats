# Adapt spatial transformation matrix for 1-based indices.

Adapt spatial transformation matrix for 1-based indices.

## Usage

``` r
sm0to1(tf_matrix)
```

## Arguments

- tf_matrix:

  4x4 numerical matrix, the input spatial transformation matrix,
  suitable for 0-based indices. Typically this is a vox2ras matrix
  obtained from functions like
  [`mghheader.vox2ras`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md).

## Value

4x4 numerical matrix, adapted spatial transformation matrix, suitable
for 1-based indices.

## See also

[`sm1to0`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)
for the inverse operation

Other header coordinate space:
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
[`mghheader.vox2ras.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
[`read.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.md),
[`read.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)
