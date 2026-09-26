# Parse the diffusion metadata of a NRRD header.

The teem/DTI-TK/3D Slicer convention for diffusion MRI data in NRRD
files stores the b-value in the field 'DWMRI_b-value' and one gradient
vector per volume in the fields 'DWMRI_gradient_0000',
'DWMRI_gradient_0001' and so on. The optional 'measurement frame' field
is the rotation that maps the gradient vectors, which are given in the
image (voxel) frame, into the frame in which the gradients were
measured. The values are returned exactly as they are stored, i.e. in
the image frame, together with the measurement frame, so that a caller
can apply it (or hand the values to the gradient table functions of this
package, which expect gradients in image axes as well).

## Usage

``` r
nrrd.dwi.info(fields, num_volumes)
```

## Arguments

- fields:

  named list, the parsed header fields.

- num_volumes:

  integer, the number of volumes (the size of the last axis), used to
  check the number of gradients.

## Value

named list with entries `b_value` (numeric or NULL), `bvec` (numeric
matrix with one row per gradient, or NULL), `num_gradients` (integer, 0
when there are none) and `measurement_frame` (3x3 numeric matrix or
NULL). Returns NULL when the header contains no DWI information at all.
