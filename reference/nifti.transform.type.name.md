# Get the name of the transform type from a form code.

The form code is a code stored in the `sform_code` and/or `qform_code`
NIFTI header fields.

## Usage

``` r
nifti.transform.type.name(form_code)
```

## Arguments

- form_code:

  integer, the value retrieved from the `sform_code` or the `qform_code`
  NIFTI header fields

## Value

character string, the meaning of the code. Usually this expresses to
what the data will be aligned after application of the vox2ras
transformation method. (The type of transformation to perform in order
to achieve this alignment depends on whether the value was retrieved
from the `sform` or the `qform` field and does not matter here.)
