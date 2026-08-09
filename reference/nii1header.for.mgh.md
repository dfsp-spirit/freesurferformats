# Create a NIFTI v1 header from the header information contained in an fs.volume instance.

Create a NIFTI v1 header from the header information contained in an
fs.volume instance.

## Usage

``` r
nii1header.for.mgh(mgh, endian = "little")
```

## Arguments

- mgh:

  an `fs.volume` instance, or a string. If a string, it is interpreted
  as a filepath to a volume file (NIFTI, MGH or MGZ) that will be loaded
  with
  [`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md).

- endian:

  character string, the endianness to use. Either 'little' or 'big'.
  Defaults to 'little'.

## Value

a NIFTI v1 header structure (see
[`ni1header.template`](https://dfsp-spirit.github.io/freesurferformats/reference/ni1header.template.md)).
Note that the header may or may not contain full RAS information,
depending on whether the source `fs.volume` contained such information
or not. If the MGH header does not have valid RAS information, the qform
and sform codes will be set to 0 (unknown).

## Note

This is intended to be used with
[`write.nifti1`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti1.md),
which allows users to convert MGH/MGZ data to NIFTI files.

## See also

Other nifti1 writers:
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md),
[`write.nifti1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti1.md)
