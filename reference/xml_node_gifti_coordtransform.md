# Create XML GIFTI CoordinateSystemTransformMatrix node.

Create XML GIFTI CoordinateSystemTransformMatrix node.

## Usage

``` r
xml_node_gifti_coordtransform(
  transform_matrix,
  data_space = "NIFTI_XFORM_UNKNOWN",
  transformed_space = "NIFTI_XFORM_UNKNOWN",
  as_cdata = TRUE
)
```

## Arguments

- transform_matrix:

  numerical 4x4 matrix, the transformation matrix from 'data_space' to
  'transformed_space'.

- data_space:

  character string, the space used by the data before transformation.

- transformed_space:

  character string, the space reached after application of the
  transformation matrix.

- as_cdata:

  logical, whether to wrap text attributes ('data_space' and
  'transformed_space') in cdata tags.

## Value

XML node from xml2
