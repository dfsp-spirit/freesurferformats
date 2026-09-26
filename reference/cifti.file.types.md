# The standard CIFTI-2 file types.

The nine standard CIFTI-2 file types, as defined by the format: which
matrix index types the two matrix dimensions must have, the file name
extension, and the NIFTI intent code and name that identify the type in
the file header. The extension of a file decides which entry is used
when writing, and a mismatch between the file name and the axes of the
data is an error.

## Usage

``` r
cifti.file.types()
```

## Value

a data.frame with one row per file type and the columns 'extension',
'intent_code', 'intent_name', 'dim0_type' and 'dim1_type'.
