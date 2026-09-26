# Consume bytes from a VTK legacy reader.

Consume bytes from a VTK legacy reader.

## Usage

``` r
vtk.reader.bytes(reader, num_bytes)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- num_bytes:

  single non-negative integer, the number of bytes to consume.

## Value

raw vector of length `num_bytes`.
