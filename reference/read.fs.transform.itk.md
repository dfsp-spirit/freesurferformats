# Read a transformation matrix from an ITK text transform file.

Read the plain text file format that ITK and the tools built on it (3D
Slicer, ANTs via `ConvertTransformFile`, SimpleITK and the workflows of
fMRIPrep/QSIPrep that are based on them) use to store linear
transformations, usually with the extension `.tfm` or `.txt`.

## Usage

``` r
read.fs.transform.itk(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

an `fs.transform` instance. An ITK transform operates on the world
coordinates of the images, which in ITK are left-posterior-superior, so
`space_in` and `space_out` are 'lps' and `voxel_base` is `NA`. This is
not the RAS space that the other formats of this package use, and it is
not converted silently: use
[`transform2ras`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2ras.md)
to get a transformation in RAS coordinates. The volumes are not recorded
in the file, so `src` and `dst` are `NULL`. The ITK class name (e.g.
'AffineTransform_float_3_3') is stored in the `type` field, and the
values of the `FixedParameters` entry in the `fixed_parameters` field.

## Note

The format can store many kinds of transforms besides affine ones; this
function reads the affine transformations only, i.e. the classes
'AffineTransform_float_3_3', 'AffineTransform_double_3_3',
'MatrixOffsetTransformBase_float_3_3' and
'MatrixOffsetTransformBase_double_3_3'. These are the classes that occur
in the output of the pipelines mentioned above, and the only ones for
which the interpretation of the parameters could be verified against
other implementations. Files that contain several transformations (an
ITK 'CompositeTransform') are not supported either, and are reported as
such: composing them requires the ordering rules of ITK, which would be
a guess without a reference to check against.

The `FixedParameters` entry is the center of rotation, so the matrix
that is returned is `y = A(x - c) + t + c`, i.e. it has the center
folded in. That is the same thing that the ITK writer of this package
stores, and the transformation is not changed by it.

FreeSurfer reads ITK files as well, but two limitations of its version
7.4.1 are worth knowing when the file has to be passed to it: it rejects
the 'float' variant of the classes ('readITK: Transform type unknown!'),
and its `lta_convert --initk` ignores the `FixedParameters`, so it
interprets a file with a non-zero center of rotation differently from
ITK itself (which computes
`offset = translation + center - matrix * center`, see `ComputeOffset()`
in ITK's `itkMatrixOffsetTransformBase.hxx`) and from this package. Both
were verified by converting files that encode the same transformation,
and both are avoided by the files that
[`write.fs.transform.itk`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md)
writes.

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
xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
tf <- transform2ras(transform2lps(read.fs.transform(xfm_file)))
summary(tf)$space_in
#> [1] "ras"
```
