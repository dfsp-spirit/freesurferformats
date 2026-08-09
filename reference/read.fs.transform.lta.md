# Load transformation matrix from a FreeSurfer linear transform array (LTA) file.

Load transformation matrix from a FreeSurfer linear transform array
(LTA) file.

## Usage

``` r
read.fs.transform.lta(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

4x4 numerical matrix, the transformation matrix

## Note

I found no spec for the LTA file format, only example files, so this
function should be used with care. If you have a file that is not parsed
correctly, please open an issue and attach it.

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
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md)

## Examples

``` r
tf_file <- system.file("extdata", "talairach.lta",
  package = "freesurferformats", mustWork = TRUE
)
transform <- read.fs.transform.lta(tf_file)
transform$matrix
#>             [,1]         [,2]        [,3]      [,4]
#> [1,] 1.054775715  0.007081697 -0.05151368 -2.029556
#> [2,] 0.005556736  1.116058946  0.33849043 -5.882306
#> [3,] 0.060359493 -0.274788439  0.85348159 37.464622
#> [4,] 0.000000000  0.000000000  0.00000000  1.000000
```
