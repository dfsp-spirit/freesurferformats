# Check that all cells of the requested type are triangles.

Check that all cells of the requested type are triangles.

## Usage

``` r
vtk.check.triangles(cells, filepath)
```

## Arguments

- cells:

  list of integer vectors, as returned by
  [`vtk.parse.cell.section`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.parse.cell.section.md).

- filepath:

  character string, the file the cells were read from.

## Value

`NULL`, invisibly. Stops if a cell is not a triangle.
