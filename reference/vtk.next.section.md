# Read the next section header line of a VTK legacy file.

Read the next section header line of a VTK legacy file.

## Usage

``` r
vtk.next.section(reader)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

## Value

named list with the entries 'keyword' (character, e.g. 'POLYGONS') and
'args' (character vector, the remaining tokens of the line), or `NULL`
at the end of the file.
