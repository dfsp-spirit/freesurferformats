# Load transformation matrix from a tkregister dat file.

Load transformation matrix from a tkregister dat file.

## Usage

``` r
read.fs.transform.dat(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

4x4 numerical matrix, the transformation matrix

## See also

Other header coordinate space:
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
[`mghheader.vox2ras.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
[`read.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)

## Examples

``` r
tf_file <- system.file("extdata", "register.dat",
  package = "freesurferformats",
  mustWork = TRUE
)
transform <- read.fs.transform.dat(tf_file)
transform$matrix
#>              [,1]         [,2]        [,3]         [,4]
#> [1,]  0.999869585  0.006901878  0.01459838   0.08490597
#> [2,] -0.014461628 -0.019486522  0.99970520 -17.40991402
#> [3,]  0.007197575 -0.999786258 -0.01938400  -7.02687693
#> [4,]  0.000000000  0.000000000  0.00000000   1.00000000
```
