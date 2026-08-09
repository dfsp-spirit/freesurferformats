# Get GIFTI XML representation of data.

Creates a GIFTI XML tree from your datasets (vectors and matrices). The
tree can be further modified to add additional data, or written to a
file as is to produce a valid GIFTI file (see
[`gifti_xml_write`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_xml_write.md)).

## Usage

``` r
gifti_xml(
  data_array,
  intent = "NIFTI_INTENT_SHAPE",
  datatype = "NIFTI_TYPE_FLOAT32",
  encoding = "GZipBase64Binary",
  endian = "LittleEndian",
  transform_matrix = NULL,
  force = FALSE
)
```

## Arguments

- data_array:

  list of data vectors and/or data matrices.

- intent:

  vector of NIFTI intent strings for the data vectors in 'data_array'
  parameter, see
  [`convert_intent`](https://rdrr.io/pkg/gifti/man/convert_intent.html).
  Example: 'NIFTI_INTENT_SHAPE'. See
  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group\_\_NIFTI1\_\_INTENT\_\_CODES.html.

- datatype:

  vector of NIFTI datatype strings. Example: 'NIFTI_TYPE_FLOAT32'.
  Should be suitable for your data.

- encoding:

  vector of encoding definition strings. One of 'ASCII', 'Base64Binary',
  'GZipBase64Binary'.

- endian:

  vector of endian definition strings. One of 'LittleEndian' or
  'BigEndian'. See
  [`convert_endian`](https://rdrr.io/pkg/gifti/man/convert_endian.html).

- transform_matrix:

  optional, a list of transformation matrices, one for each data_array.
  If one of the data arrays has none, pass `NA`. Each transformation
  matrix in the outer list has to be a 4x4 matrix or given as a named
  list with entries 'transform_matrix', 'data_space', and
  'transformed_space'. Here is an example:
  `list('transform_matrix'=diag(4), 'data_space'='NIFTI_XFORM_UNKNOWN', 'transformed_space'='NIFTI_XFORM_UNKNOWN')`.

- force:

  logical, whether to force writing the data, even if issues like a
  mismatch of datatype and data values are detected.

## Value

xml tree, see xml2 package. One could modify this tree as needed using
xml2 functions, e.g., add metadata.

## Note

Unless you want to modify the returned tree manually, you should not
need to call this function. Use
[`gifti_writer`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_writer.md)
instead.

## References

See https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf

## See also

The example for
[`gifti_xml_write`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_xml_write.md)
shows how to modify the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
my_data_sets <- list(rep(3.1, 3L), matrix(seq(6) + 0.1, nrow = 2L))
transforms <- list(NA, list(
  "transform_matrix" = diag(4), "data_space" = "NIFTI_XFORM_UNKNOWN",
  "transformed_space" = "NIFTI_XFORM_UNKNOWN"
))
xmltree <- gifti_xml(my_data_sets, datatype = "NIFTI_TYPE_FLOAT32", transform_matrix = transforms)
# Verify that the tree is a valid GIFTI file:
gifti_xsd <- "https://www.nitrc.org/frs/download.php/158/gifti.xsd"
xml2::xml_validate(xmltree, xml2::read_xml(gifti_xsd))
} # }
```
