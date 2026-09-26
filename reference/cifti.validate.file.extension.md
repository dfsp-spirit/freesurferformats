# Check the file extension against the axes of the data.

A CIFTI-2 file name states the file type (`.dscalar`, `.pdconn`, ...),
and the type decides which matrix index types the two dimensions have. A
file whose name contradicts its content is silently misread by other
software, so this is an error.

## Usage

``` r
cifti.validate.file.extension(filepath, file_type)
```

## Arguments

- filepath:

  character string, the file name to write.

- file_type:

  a one row data.frame, see
  [`cifti.file.type.for.axes`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.file.type.for.axes.md).

## Value

`NULL`, invisibly. Stops if the file name names a different file type.
