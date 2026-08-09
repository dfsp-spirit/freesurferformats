# Load transformation matrix from an XFM file.

Load transformation matrix from an XFM file.

## Usage

``` r
read.fs.transform.xfm(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

4x4 numerical matrix, the transformation matrix

## Note

Currently this function has been tested with linear transformation files
only, all others are unsupported.

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
[`read.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)

## Examples

``` r
tf_file <- system.file("extdata", "talairach.xfm",
  package = "freesurferformats",
  mustWork = TRUE
)
transform <- read.fs.transform.xfm(tf_file)
transform$matrix
#>           [,1]      [,2]     [,3]       [,4]
#> [1,]  1.111536  0.040948 0.012535  -0.803558
#> [2,] -0.029730  0.981154 0.342306 -19.558083
#> [3,]  0.022961 -0.452588 1.111222  10.044540
#> [4,]  0.000000  0.000000 0.000000   1.000000
```
