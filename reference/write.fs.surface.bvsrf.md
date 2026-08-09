# Write surface to Brainvoyager SRF file.

Write surface to Brainvoyager SRF file.

## Usage

``` r
write.fs.surface.bvsrf(
  filepath,
  vertex_coords,
  faces,
  normals = NULL,
  neighborhoods = NULL
)
```

## Arguments

- filepath:

  string. Full path to the output curv file. If it ends with ".gz", the
  file is written in gzipped format. Note that this is not common, and
  that other software may not handle this transparently.

- vertex_coords:

  n x 3 matrix of doubles. Each row defined the x,y,z coords for a
  vertex.

- faces:

  n x 3 matrix of integers. Each row defined the 3 vertex indices that
  make up the face. WARNING: Vertex indices should be given in R-style,
  i.e., the index of the first vertex is 1. However, they will be
  written in FreeSurfer style, i.e., all indices will have 1
  substracted, so that the index of the first vertex will be zero.

- normals:

  matrix of nx3 vertex normals (x,y,z)

- neighborhoods:

  list of integer lists, the indices of the nearest neighbors for each
  vertex (an adjacency list). The sub list at index n contains the
  indices of the vertices in the 1-neighborhood of vertex n. The vertex
  indices in the sub lists must be zero-based.

## Note

This function is experimental. Only SRF file format version 4 is
supported.
