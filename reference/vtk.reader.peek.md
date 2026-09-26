# Read bytes from a VTK legacy reader without consuming them.

Read bytes from a VTK legacy reader without consuming them.

## Usage

``` r
vtk.reader.peek(reader, num_bytes)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- num_bytes:

  single non-negative integer, the number of bytes to peek at.

## Value

raw vector, possibly shorter than `num_bytes` at the end of the file.
