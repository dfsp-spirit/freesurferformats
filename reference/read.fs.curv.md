# Read file in FreeSurfer curv format

Read vertex-wise brain morphometry data from a file in FreeSurfer 'curv'
format. Both the binary and ASCII versions are supported. For a subject
(MRI image pre-processed with FreeSurfer) named 'bert', an example file
would be 'bert/surf/lh.thickness', which contains n values. Each value
represents the cortical thickness at the respective vertex in the brain
surface mesh of bert.

## Usage

``` r
read.fs.curv(filepath, format = "auto", with_header = FALSE)
```

## Arguments

- filepath:

  string. Full path to the input curv file. Note: gzipped binary curv
  files are supported and gz binary format is assumed if the filepath
  ends with ".gz".

- format:

  one of 'auto', 'asc', 'bin', 'nii' or 'txt'. The format to assume. If
  set to 'auto' (the default), binary format will be used unless the
  filepath ends with '.asc' or '.txt'. The latter is just one float
  value per line in a text file.

- with_header:

  logical, whether to return named list with 'header' and 'data' parts.
  Only valid with FreeSurfer binary curv format.

## Value

data vector of floats. The brain morphometry data, one value per vertex.

## See also

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
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
ct <- read.fs.curv(curvfile)
cat(sprintf(
  "Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
  length(ct), min(ct), mean(ct), max(ct)
))
#> Read data for 149244 vertices. Values: min=0.000000, mean=2.437466, max=5.000000.
```
