# Build the NIFTI-2 header of a CIFTI-2 file.

CIFTI-2 files are NIFTI-2 files whose header has a fixed shape: the
matrix dimensions are stored in the `dim` field (dimension 0 in
`dim[5]`, dimension 1 in `dim[6]`, which is why `dim[0]` is 6 for a
two-dimensional matrix), the intent code names the file type, the voxel
sizes, the scaling and the geometry fields are unused (the geometry of a
CIFTI-2 file is in its XML), and the data start after the header
extension that holds the XML.

## Usage

``` r
cifti.nifti.header.for.axes(axes, intent_code, intent_name)
```

## Arguments

- axes:

  list of axes, see
  [`cifti.header.from.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md).

- intent_code:

  integer, the NIFTI intent code of the file type, see
  [`cifti.file.types`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.file.types.md).

- intent_name:

  character string, the NIFTI intent name of the file type.

## Value

a named list, a NIFTI-2 header as returned by
[`read.nifti2.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md).
