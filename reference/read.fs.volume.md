# Read volume file in MGH, MGZ or NIFTI format

Read multi-dimensional brain imaging data from a file.

## Usage

``` r
read.fs.volume(
  filepath,
  format = "auto",
  flatten = FALSE,
  with_header = FALSE,
  drop_empty_dims = FALSE
)
```

## Arguments

- filepath:

  string. Full path to the input MGZ, MGH or NIFTI file.

- format:

  character string, one one of 'auto', 'nii', 'mgh' or 'mgz'. The format
  to assume. If set to 'auto' (the default), the format will be derived
  from the file extension.

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
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
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
vd <- read.fs.volume(brain_image)
cat(sprintf(
  "Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
  paste(dim(vd), collapse = " "), min(vd), mean(vd), max(vd)
))
#> Read voxel data with dimensions 256 256 256 1. Values: min=0, mean=7.214277, max=156.
# Read it again with full header data:
vdh <- read.fs.volume(brain_image, with_header = TRUE)
# Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
vox2ras_matrix <- mghheader.vox2ras(vdh)
vox2ras_matrix %*% c(0, 0, 0, 1)
#>           [,1]
#> [1,] 127.50005
#> [2,] -98.62726
#> [3,]  79.09527
#> [4,]   1.00000
```
