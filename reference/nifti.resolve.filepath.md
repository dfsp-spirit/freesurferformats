# Resolve the path of a NIFTI file which is given without a file extension.

Resolve the path of a NIFTI file which is given without a file
extension.

## Usage

``` r
nifti.resolve.filepath(filepath)
```

## Arguments

- filepath:

  character string, the path to a NIFTI file.

## Value

character string, the path of an existing file. If `filepath` itself
does not exist, the usual NIFTI file extensions are appended to it and
the first existing file is returned.
