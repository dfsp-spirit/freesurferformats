# Write an MNI transform (xfm) file.

Write a transformation in the FreeSurfer xfm format, the format of
`talairach.xfm` files, which store the transformation from the RAS space
of a subject to the RAS space of an MNI or Talairach template. Only the
first three rows of the matrix are stored, so the transformation must be
affine, and it must map RAS coordinates to RAS coordinates.

## Usage

``` r
write.fs.transform.xfm(tf, filepath, type = "Linear")
```

## Arguments

- tf:

  an `fs.transform` instance whose matrix maps RAS coordinates to RAS
  coordinates.

- filepath:

  character string, the full path of the file to write.

- type:

  character string, the value of the `Transform_Type` entry, 'Linear' by
  default.

## Value

the `fs.transform` instance `tf`, invisibly.

## See also

Other header coordinate space:
[`invert.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/invert.fs.transform.md),
[`is.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/is.fs.transform.md),
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
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
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md)

## Examples

``` r
xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
tf <- read.fs.transform(xfm_file)
out_file <- tempfile(fileext = ".xfm")
write.fs.transform.xfm(tf, out_file)
max(abs(read.fs.transform(out_file)$matrix - tf$matrix)) # 0
#> [1] 0
unlink(out_file)
```
