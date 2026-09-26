# Write a tkregister dat file.

Write a transformation in the FreeSurfer tkregister format
(`register.dat`), the format that `mri_vol2vol --reg`, `tkregister2` and
`bbregister` use. Such a matrix maps the voxel coordinates of the
movable volume (the source) to RAS coordinates in the tkregister frame
of the target volume, see
[`mghheader.vox2ras.tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
so a transformation can only be written if this is what it maps.

## Usage

``` r
write.fs.transform.dat(
  tf,
  filepath,
  subject = NULL,
  in_plane_resolution = NULL
)
```

## Arguments

- tf:

  an `fs.transform` instance whose matrix maps voxel coordinates to RAS
  coordinates.

- filepath:

  character string, the full path of the file to write.

- subject:

  `NULL` or character string, the subject identifier to store in the
  first line of the file. This is metadata for the tools that read the
  file and does not influence the transformation. If `NULL` and `tf` has
  a `subject` field (as read by
  [`read.fs.transform.dat`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md)),
  that value is used.

- in_plane_resolution:

  `NULL` or numerical vector of length 2, the in-plane and between-plane
  resolution of the movable volume in millimeters, stored in the second
  and third line of the file. These are metadata that describe the
  acquisition and do not influence the transformation. If `NULL` and
  `tf` has the corresponding fields, they are used, otherwise they are
  derived from the geometry of the source volume, and set to 1 if that
  is not available either.

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
[`write.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.fslmat.md),
[`write.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md),
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
dat_file <- system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE)
tf <- read.fs.transform(dat_file)
out_file <- tempfile(fileext = ".dat")
write.fs.transform.dat(tf, out_file)
max(abs(read.fs.transform(out_file)$matrix - tf$matrix)) # 0
#> [1] 0
unlink(out_file)
```
