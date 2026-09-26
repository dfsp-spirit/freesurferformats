# Write a transformation matrix in FSL format.

Write a 4x4 matrix as an FSL matrix file, i.e. as the plain text file
that FSL's `flirt` writes with the `-omat` option and that FSL, MRtrix3
and FreeSurfer read as the registration between two images. The matrix
must map voxel coordinates to voxel coordinates (`space_in` and
`space_out` are 'voxel'), because that is what an FSL matrix stores: it
relates the voxel grid of the image given to `flirt -in` to the voxel
grid of the image given to `flirt -ref`, and it does not record which
images those were. Use
[`transform2voxel`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md)
to convert a transformation in world coordinates into one that can be
written.

## Usage

``` r
write.fs.transform.fslmat(tf, filepath)
```

## Arguments

- tf:

  an `fs.transform` instance whose matrix maps voxel coordinates to
  voxel coordinates.

- filepath:

  character string, the full path of the file to write.

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
[`write.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md),
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
tf_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
out_file <- tempfile(fileext = ".mat")
write.fs.transform.fslmat(read.fs.transform(tf_file), out_file)
readLines(out_file)
#> [1] "1.054775714874268 0.0070816967636346817 -0.051513675600290298 -2.0295562744140621"
#> [2] "0.0055567356757819653 1.116058945655823 0.33849042654037481 -5.8823056221008301"  
#> [3] "0.060359492897987373 -0.27478843927383417 0.85348159074783325 37.464622497558587" 
#> [4] "0 0 0 1"                                                                          
unlink(out_file)
```
