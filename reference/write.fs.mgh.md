# Write file in FreeSurfer MGH or MGZ format

Write brain data to a file in FreeSurfer binary MGH or MGZ format.

## Usage

``` r
write.fs.mgh(
  filepath,
  data,
  vox2ras_matrix = NULL,
  mr_params = c(0, 0, 0, 0, 0),
  mri_dtype = "auto"
)
```

## Arguments

- filepath:

  string. Full path to the output curv file. If this ends with ".mgz",
  the file will be written gzipped (i.e., in MGZ instead of MGH format).

- data:

  matrix or array of numerical values. The brain data to write. Must be
  integers or doubles. (The data type is set automatically to MRI_INT
  for integers and MRI_FLOAT for doubles in the MGH header).

- vox2ras_matrix:

  4x4 matrix. An affine transformation matrix for the RAS transform that
  maps voxel indices in the volume to coordinates, such that for
  y(i1,i2,i3) (i.e., a voxel defined by 3 indices in the volume), the
  xyz coordinates are `vox2ras_matrix*[i1 i2 i3 1]`. If no matrix is
  given (or a NULL value), the ras_good flag will be 0 in the file.
  Defaults to NULL.

- mr_params:

  double vector of length four (without fov) or five. The acquisition
  parameters, in order: tr, flipangle, te, ti, fov. Spelled out:
  repetition time, flip angle, echo time, inversion time, field-of-view.
  The unit for the three times is ms, the angle unit is radians.
  Defaults to c(0., 0., 0., 0., 0.) if omitted. Pass NULL if you do not
  want to write them at all.

- mri_dtype:

  character string representing an MRI data type code or 'auto'. Valid
  strings are 'MRI_UCHAR' (1 byte unsigned integer), 'MRI_SHORT' (2 byte
  signed integer), 'MRI_INT' (4 byte signed integer) and 'MRI_FLOAT' (4
  byte signed floating point). The default value `auto` will determine
  the data type from the type of the `data` parameter. It will use
  MRI_INT for integers, so you may be able to save space by manually
  settings the dtype if the range of your data does not require that.
  WARNING: If manually specified, no sanitation of any kind is
  performed. Leave this alone if in doubt.

## See also

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.curv.md),
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
[`read.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.gii.md),
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md),
[`read.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.weight.md),
[`write.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.curv.md),
[`write.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.gii.md),
[`write.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.md),
[`write.fs.morph.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.asc.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md),
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md),
[`write.fs.morph.smp()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.smp.md),
[`write.fs.morph.txt()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.txt.md),
[`write.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.md),
[`write.fs.weight.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.asc.md)
