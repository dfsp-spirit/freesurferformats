# Write an ITK text transform file.

Write a transformation in the text file format of ITK, which the tools
built on ITK read: 3D Slicer (which calls it the 'ITK Transform'
format), ANTs (via `ConvertTransformFile`), SimpleITK, and the workflows
that write their transformations with them. The file name extension is
usually `.tfm` or `.txt`.

An ITK transform operates on the world coordinates of the images, which
in ITK are left-posterior-superior, so only a transformation that maps
LPS coordinates can be written. Use
[`transform2lps`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2lps.md)
to convert a transformation in RAS coordinates.

## Usage

``` r
write.fs.transform.itk(tf, filepath)
```

## Arguments

- tf:

  an `fs.transform` instance whose matrix maps LPS coordinates to LPS
  coordinates.

- filepath:

  character string, the full path of the file to write.

## Value

the `fs.transform` instance `tf`, invisibly.

## Note

The file that is written uses the class 'AffineTransform_double_3_3',
i.e. the parameters are stored in double precision, and it states a
center of rotation of zero, with the center folded into the translation.
This is exactly the form that FreeSurfer's `lta_convert --outitk`
writes, and the form that `lta_convert --initk` can read: it rejects the
'float' variant of the classes and ignores a non-zero center of
rotation, see the note in
[`read.fs.transform.itk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.itk.md).

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
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
out_file <- tempfile(fileext = ".tfm")
write.fs.transform.itk(transform2lps(read.fs.transform(xfm_file)), out_file)
readLines(out_file)
#> [1] "#Insight Transform File V1.0"                                                                                                                                                                                                     
#> [2] "#Transform 0"                                                                                                                                                                                                                     
#> [3] "Transform: AffineTransform_double_3_3"                                                                                                                                                                                            
#> [4] "Parameters: 1.1115360000000001 0.040947999999999998 -0.012534999999999999 -0.029729999999999999 0.98115399999999997 -0.342306 -0.022960999999999999 0.45258799999999999 1.1112219999999999 0.80355799999999999 19.558083 10.04454"
#> [5] "FixedParameters: 0 0 0"                                                                                                                                                                                                           
unlink(out_file)
```
