# Read surface mesh in STL ASCII format.

The STL format is a mesh format that is often used for 3D printing, it
stores geometry information. It is known as stereolithography format. A
binary and an ASCII version exist. This function reads the ASCII
version.

## Usage

``` r
read.fs.surface.stl.ascii(filepath, digits = 6L)
```

## Arguments

- filepath:

  full path to surface mesh file in STL format.

- digits:

  the precision (number of digits after decimal separator) to use when
  determining whether two x,y,z coords define the same vertex. This is
  used when the polygon soup is turned into an indexed mesh.

## Value

an `fs.surface` instance. The normals are available in the 'metadata'
property.

## Note

The STL format does not use indices into a vertex list to define faces,
instead it repeats vertex coords in each face ('polygon soup').
Therefore, the mesh needs to be reconstructed, which requires the
`misc3d` package.

## References

See https://en.wikipedia.org/wiki/STL\_(file_format) for a format
description.
