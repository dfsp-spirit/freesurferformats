# Read numeric values from a binary VTK legacy file.

Read numeric values from a binary VTK legacy file.

## Usage

``` r
vtk.reader.numbers(reader, num_values, type_info)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- num_values:

  single non-negative integer, the number of values to read.

- type_info:

  named list, the result of
  [`vtk.data.type.info`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.data.type.info.md).

## Value

numeric or integer vector of length `num_values`.

## Note

Legacy VTK binary data is always big endian, the format has no way of
expressing a different byte order.
