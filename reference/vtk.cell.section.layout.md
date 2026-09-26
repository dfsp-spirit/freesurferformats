# Determine how the cell array of a section is stored.

The cell arrays of VTK legacy files come in two layouts, see the comment
at the top of this file. Both layouts are still written by software in
use today, and the version number in the header does not reliably
indicate which one a file uses, so the layout is detected from the
presence of the OFFSETS keyword: the old layout starts the cell data
with the vertex count of the first cell as a raw number, whereas the new
layout starts it with the string 'OFFSETS'. For ASCII files that is
simply the next line; for binary files the first byte decides, and
because a cell vertex count is a small integer, the first byte of an old
style cell array is always zero.

## Usage

``` r
vtk.cell.section.layout(reader)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

## Value

named list with the entry 'layout' ('old' or 'new') and, for the new
layout, the entry 'type' (character, the name of the offsets data type).
