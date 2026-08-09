# Read file in FreeSurfer MGH or MGZ format

Read multi-dimensional brain imaging data from a file in FreeSurfer
binary MGH or MGZ format. The MGZ format is just a gzipped version of
the MGH format. For a subject (MRI image pre-processed with FreeSurfer)
named 'bert', an example file would be 'bert/mri/T1.mgz', which contains
a 3D brain scan of bert.

## Usage

``` r
read.fs.mgh(
  filepath,
  is_gzipped = "AUTO",
  flatten = FALSE,
  with_header = FALSE,
  drop_empty_dims = FALSE
)
```

## Arguments

- filepath:

  string. Full path to the input MGZ or MGH file.

- is_gzipped:

  a logical value or the string 'AUTO'. Whether to treat the input file
  as gzipped, i.e., MGZ instead of MGH format. Defaults to 'AUTO', which
  tries to determine this from the last three characters of the
  'filepath' parameter. Files with extensions 'mgz' and '.gz' (in
  arbitrary case) are treated as MGZ format, all other files are treated
  as MGH. In the special case that 'filepath' has less than three
  characters, MGH is assumed.

- flatten:

  logical. Whether to flatten the return volume to a 1D vector. Useful
  if you know that this file contains 1D morphometry data.

- with_header:

  logical. Whether to return the header as well. If TRUE, return an
  instance of class `fs.volume` for data with at least 3 dimensions, a
  named list with entries "data" and "header". The latter is another
  named list which contains the header data. These header entries exist:
  "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT;
  4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions.
  E.g., c(256, 256, 256, 1). These header entries may exist:
  "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists
  if "has_mr_params" is 1). See the `mghheader.*` functions, like
  [`mghheader.vox2ras.tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
  to compute more information from the header fields.

- drop_empty_dims:

  logical, whether to drop empty dimensions of the returned data

## Value

data, multi-dimensional array. The brain imaging data, one value per
voxel. The data type and the dimensions depend on the data in the file,
they are read from the header. If the parameter flatten is `TRUE`, a
numeric vector is returned instead. Note: The return value changes if
the parameter with_header is `TRUE`, see parameter description.

## See also

To derive more information from the header, see the `mghheader.*`
functions, like
[`mghheader.vox2ras.tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md).

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.curv.md),
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
brain_image <- system.file("extdata", "brain.mgz",
  package = "freesurferformats",
  mustWork = TRUE
)
vd <- read.fs.mgh(brain_image)
cat(sprintf(
  "Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
  paste(dim(vd), collapse = " "), min(vd), mean(vd), max(vd)
))
#> Read voxel data with dimensions 256 256 256 1. Values: min=0, mean=7.214277, max=156.
# Read it again with full header data:
vdh <- read.fs.mgh(brain_image, with_header = TRUE)
# Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
vdh$header$vox2ras_matrix %*% c(0, 0, 0, 1)
#>           [,1]
#> [1,] 127.50005
#> [2,] -98.62726
#> [3,]  79.09527
#> [4,]   1.00000
```
