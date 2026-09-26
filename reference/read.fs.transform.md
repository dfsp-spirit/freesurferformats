# Load transformation matrix from a file.

Load transformation matrix from a file.

## Usage

``` r
read.fs.transform(filepath, format = "auto")
```

## Arguments

- filepath:

  character string, the full path to the transform file.

- format:

  character string, the file format. 'auto' guesses it from the file
  extension and the file content, and 'xfm' (for xform format), 'dat'
  (for tkregister style, e.g. register.dat), 'lta' (for FreeSurfer LTA),
  'fslmat' (for an FSL/FLIRT matrix) and 'itk' (for an ITK text
  transform, e.g. a `.tfm` file) can be given explicitly.

## Value

an `fs.transform` instance, see
[`fs.transform`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.transform.md).
Its fields include the 'matrix', and the coordinate spaces the matrix
maps between (`space_in`, `space_out` and `voxel_base`). Which of them
are known depends on the format: an xfm file states neither (both sides
are RAS), a register.dat file states both by definition, an FSL matrix
maps voxel coordinates (zero-based) to voxel coordinates, and an LTA
file states the spaces in its header.

## Note

Currently this function has been tested with linear transformation files
only, all others are unsupported.

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
tf_file <- system.file("extdata", "talairach.xfm",
  package = "freesurferformats",
  mustWork = TRUE
)
transform <- read.fs.transform(tf_file)
transform$matrix
#>           [,1]      [,2]     [,3]       [,4]
#> [1,]  1.111536  0.040948 0.012535  -0.803558
#> [2,] -0.029730  0.981154 0.342306 -19.558083
#> [3,]  0.022961 -0.452588 1.111222  10.044540
#> [4,]  0.000000  0.000000 0.000000   1.000000
```
