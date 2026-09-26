# Read a VTK legacy file with a POLYDATA dataset.

Reads the geometry (points and the cell arrays VERTICES, LINES and
POLYGONS) of a VTK legacy file. Both the ASCII and the binary encoding
are supported, as are the old and the new cell array layout, see the
comment at the top of the file. Attribute data such as normals, texture
coordinates or scalars is ignored.

## Usage

``` r
read.vtk.legacy.polydata(filepath)
```

## Arguments

- filepath:

  character string, path to the VTK file.

## Value

named list with the entries 'version' (character, the VTK version from
the header), 'encoding' (character, 'ASCII' or 'BINARY'), 'points' (n x
3 double matrix, or NULL), 'verts', 'lines' and 'polys' (each a list of
integer vectors with 0-based vertex indices, or NULL if the file does
not contain that cell type), and 'ignored_sections' (character vector
with the keywords of the sections that were not read, e.g.
'POINT_DATA').
