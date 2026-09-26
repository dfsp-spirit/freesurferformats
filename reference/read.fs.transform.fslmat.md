# Read a transformation matrix from an FSL matrix file.

Read the plain text 4x4 matrix that FSL's `flirt` writes with the
`-omat` option, and that FSL, MRtrix3 and FreeSurfer read as the
registration between two images.

## Usage

``` r
read.fs.transform.fslmat(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

an `fs.transform` instance. An FSL matrix maps the voxel coordinates of
the image given to `flirt -in` to those of the image given to
`flirt -ref`. Both are voxel indices, so `space_in` and `space_out` are
'voxel' and `voxel_base` is 0 (FSL voxel indices are zero-based). The
two images are not recorded in the file, so `src` and `dst` are `NULL`
and the volumes have to be passed to
[`transform2world`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2world.md)
to interpret the matrix in world coordinates.

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
# Write the example LTA as an FSL matrix and read it back: no FSL installation is needed for that.
lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
mat_file <- tempfile(fileext = ".mat")
write.fs.transform(read.fs.transform(lta_file), mat_file, format = "fslmat")
read.fs.transform(mat_file)$matrix
#>             [,1]         [,2]        [,3]      [,4]
#> [1,] 1.054775715  0.007081697 -0.05151368 -2.029556
#> [2,] 0.005556736  1.116058946  0.33849043 -5.882306
#> [3,] 0.060359493 -0.274788439  0.85348159 37.464622
#> [4,] 0.000000000  0.000000000  0.00000000  1.000000
unlink(mat_file)
```
