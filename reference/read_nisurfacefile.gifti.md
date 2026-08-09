# Read a gifti file as a surface.

Read a gifti file as a surface.

## Usage

``` r
# S3 method for class 'gifti'
read_nisurfacefile(filepath, ...)
```

## Arguments

- filepath:

  character string, the full path to the input surface file.

- ...:

  ignored

## Value

an instance of `fs.surface`, read from the file. See
[`read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md)
for details. If none of the reader methods succeed, an error is raised.
