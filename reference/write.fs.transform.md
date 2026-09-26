# Write a transformation matrix to a file.

Save an `fs.transform` instance in one of the supported transformation
file formats.

A transformation stores its matrix together with the coordinate spaces
it maps between, and the formats disagree about which spaces they can
express. A transformation is only written if the format can represent it
exactly, because a silent conversion would change the meaning of the
matrix: FSL matrix files, for example, store voxel-to-voxel matrices, so
a transformation in world coordinates must be converted first with
[`transform2voxel`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md).
A format that cannot express the transformation at all is an error, not
a warning.

## Usage

``` r
write.fs.transform(tf, filepath, format = "auto")
```

## Arguments

- tf:

  an `fs.transform` instance, the transformation to write.

- filepath:

  character string, the full path of the file to write.

- format:

  character string, the file format, one of 'auto' (guess from the file
  extension), 'fslmat' (an FSL/FLIRT matrix file, i.e. a plain text 4x4
  matrix as written by FSL's `flirt -omat`), 'lta'
  ([`write.fs.transform.lta`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md)),
  'dat'
  ([`write.fs.transform.dat`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.dat.md)),
  'xfm'
  ([`write.fs.transform.xfm`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md))
  or 'itk'
  ([`write.fs.transform.itk`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md)).

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
[`write.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.dat.md),
[`write.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.fslmat.md),
[`write.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md),
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
tf <- read.fs.transform(system.file("extdata", "talairach.lta",
  package = "freesurferformats", mustWork = TRUE
))
out_file <- tempfile(fileext = ".mat")
# An LTA of type 0 is a voxel-to-voxel transformation, so it can be written as an FSL matrix.
write.fs.transform(tf, out_file, format = "fslmat")
read.fs.transform(out_file)$matrix
#>             [,1]         [,2]        [,3]      [,4]
#> [1,] 1.054775715  0.007081697 -0.05151368 -2.029556
#> [2,] 0.005556736  1.116058946  0.33849043 -5.882306
#> [3,] 0.060359493 -0.274788439  0.85348159 37.464622
#> [4,] 0.000000000  0.000000000  0.00000000  1.000000
unlink(out_file)

# The same transformation can be written in the FreeSurfer formats of the spaces it maps between.
out_file <- tempfile(fileext = ".lta")
write.fs.transform(tf, out_file)
unlink(out_file)
```
