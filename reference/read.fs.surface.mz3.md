# Read surface mesh in mz3 format, used by Surf-Ice.

The mz3 format is a binary file format that can store a mesh (vertices
and faces), and optionally per-vertex colors or scalars.

## Usage

``` r
read.fs.surface.mz3(filepath)
```

## Arguments

- filepath:

  full path to surface mesh file in mz3 format.

## Value

an `fs.surface` instance. If the mz3 file contained RGBA per-vertex
colors or scalar per-vertex data, these are available in the 'metadata'
property.

## References

See https://github.com/neurolabusc/surf-ice for details on the format.
