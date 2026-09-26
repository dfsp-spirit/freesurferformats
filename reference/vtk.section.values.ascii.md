# Read numeric values from an ASCII VTK legacy section.

The ASCII encoding does not guarantee how many values share a line, so
values are collected line by line until the expected number of them has
been read. A line that contains more values than the section declares is
treated as an error, because it means that the section header lied about
the size of the section and the file cannot be parsed reliably.

## Usage

``` r
vtk.section.values.ascii(reader, num_values)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- num_values:

  single non-negative integer, the number of values to read.

## Value

numeric vector of length `num_values`.
