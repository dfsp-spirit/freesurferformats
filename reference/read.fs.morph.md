# Read morphometry data file in any FreeSurfer format.

Read vertex-wise brain surface data from a file. The file can be in any
of the supported formats, and the format will be determined from the
file extension.

## Usage

``` r
read.fs.morph(filepath, format = "auto")
```

## Arguments

- filepath:

  string. Full path to the input file. The suffix determines the
  expected format as follows: ".mgz" and ".mgh" will be read with the
  read.fs.mgh function, all other file extensions will be read with the
  read.fs.curv function.

- format:

  character string, the format to use. One of c("auto", "mgh", "mgz",
  "curv", "gii"). The default setting "auto" will determine the format
  from the file extension.

## Value

data, vector of floats. The brain morphometry data, one value per
vertex.

## See also

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.curv.md),
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md),
[`read.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.weight.md),
[`write.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.curv.md),
[`write.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.gii.md),
[`write.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.mgh.md),
[`write.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.md),
[`write.fs.morph.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.asc.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md),
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md),
[`write.fs.morph.smp()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.smp.md),
[`write.fs.morph.txt()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.txt.md),
[`write.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.md),
[`write.fs.weight.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.asc.md)

## Examples

``` r
curvfile <- system.file("extdata", "lh.thickness",
  package = "freesurferformats", mustWork = TRUE
)
ct <- read.fs.morph(curvfile)
cat(sprintf(
  "Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
  length(ct), min(ct), mean(ct), max(ct)
))
#> Read data for 149244 vertices. Values: min=0.000000, mean=2.437466, max=5.000000.
mghfile <- system.file("extdata", "lh.curv.fwhm10.fsaverage.mgz",
  package = "freesurferformats", mustWork = TRUE
)
curv <- read.fs.morph(mghfile)
cat(sprintf(
  "Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
  length(ct), min(ct), mean(ct), max(ct)
))
#> Read data for 149244 vertices. Values: min=0.000000, mean=2.437466, max=5.000000.
```
