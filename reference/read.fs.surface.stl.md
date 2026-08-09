# Read mesh in STL format, auto-detecting ASCII versus binary format version.

Read mesh in STL format, auto-detecting ASCII versus binary format
version.

## Usage

``` r
read.fs.surface.stl(filepath, digits = 6L, is_ascii = "auto")
```

## Arguments

- filepath:

  full path to surface mesh file in STL format.

- digits:

  the precision (number of digits after decimal separator) to use when
  determining whether two x,y,z coords define the same vertex. This is
  used when the polygon soup is turned into an indexed mesh.

- is_ascii:

  logical, whether the file is in the ASCII version of the STL format
  (as opposed to the binary version). Can also be the character string
  'auto', in which case the function will try to auto-detect the format.

## Value

an `fs.surface` instance, the mesh.

## Note

The mesh is stored in the file as a polygon soup, which is transformed
into an index mesh by this function.
