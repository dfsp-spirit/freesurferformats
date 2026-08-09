# Read a FreeSurfer ASCII surface file.

Read a FreeSurfer ASCII surface file.

## Usage

``` r
# S3 method for class 'fsnative'
read_nisurfacefile(filepath, ...)
```

## Arguments

- filepath:

  character string, the full path to the input surface file.

- ...:

  parameters passed to
  [`read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md).

## Value

an instance of `fs.surface`, read from the file. See
[`read.fs.surface`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.md)
for details. If none of the reader methods succeed, an error is raised.
