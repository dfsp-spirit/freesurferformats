# Read the header information and the data of a NIFTI file into a plain list.

This is the file reading part of
[`read.fs.volume.nii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.nii.md).
The returned list uses plain entry names instead of the `oro.nifti` slot
names, so that no code outside of this file depends on the name of an
`oro.nifti` slot. The data scaling fields of the NIFTI header are
applied, see the details in
[`read.fs.volume.nii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.nii.md).

## Usage

``` r
nifti.info.from.file(filepath)
```

## Arguments

- filepath:

  character string, the path to a NIFTI v1 or v2 file. The file
  extension may be omitted, see
  [`nifti.resolve.filepath`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.resolve.filepath.md).

## Value

named list with the NIFTI header fields `magic`, `datatype`, `bitpix`,
`dim` (the 8 entry NIFTI `dim` field, its first entry the number of used
dimensions), `glmin` (the field used to store the true dimension for the
FreeSurfer hack files, `-1` for NIFTI v2 files, which do not have it),
`scl_slope`, `scl_inter`, `xyzt_units`, `pixdim` (the 8 entry NIFTI
`pixdim` field, its first entry `qfac`), `sform_code`, `srow_x`,
`srow_y`, `srow_z`, `qform_code`, `quatern_b`, `quatern_c`, `quatern_d`,
`qoffset_x`, `qoffset_y`, `qoffset_z`, and the data array in `data`.

## See also

[`nifti.info.from.oro.instance`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.info.from.oro.instance.md)
