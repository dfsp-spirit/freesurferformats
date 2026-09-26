# Read the cell array of a VTK legacy section.

Read the cell array of a VTK legacy section.

## Usage

``` r
vtk.parse.cell.section(reader, section)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

- section:

  named list, a section as returned by
  [`vtk.next.section`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.next.section.md).

## Value

list of integer vectors, one per cell, containing the 0-based vertex
indices of the cell.
