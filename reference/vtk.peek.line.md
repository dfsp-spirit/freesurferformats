# Look at the next line of an ASCII VTK legacy file.

Look at the next line of an ASCII VTK legacy file.

## Usage

``` r
vtk.peek.line(reader)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

## Value

character string, or `NULL` at the end of the file. The line is not
consumed, the next call to
[`vtk.next.line`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.next.line.md)
returns it.
