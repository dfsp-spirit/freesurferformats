# Fill the lookahead buffer of a VTK legacy reader.

Fill the lookahead buffer of a VTK legacy reader.

## Usage

``` r
vtk.reader.fill(reader, num_bytes)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- num_bytes:

  single non-negative integer, the number of bytes the buffer should
  hold.

## Value

`NULL`, invisibly. The buffer may hold fewer bytes than requested if the
file ends.
