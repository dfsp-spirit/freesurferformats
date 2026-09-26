# Parse the optionally present VoxelIndicesIJK child element as a matrix.

Parse the optionally present VoxelIndicesIJK child element as a matrix.

## Usage

``` r
cifti.parse.child.voxel.indices(node, filepath = "")
```

## Arguments

- node:

  an xml2 node, the parent element.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

an n x 3 integer matrix of 0-based voxel indices, or `NULL` if the child
element is missing or empty.
