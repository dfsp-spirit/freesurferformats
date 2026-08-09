# Adjust integer matrix to target min value.

This takes a matrix of integers, and adjusts the values such that the
minimal value is the 'target_min_index' value. It is used to adjust from
0-based to 1-based indices in meshes.

## Usage

``` r
adjust.face.indices.to(faces, target_min_index = 1L)
```

## Arguments

- faces:

  3xn integer matrix, the vertex indices of the faces

- target_min_index:

  integer, one of 1L or 0L. The target minimal value that the data
  should have afterwards.

## Value

3xn integer matrix, the adjusted values

## Note

The current and the target min values must be 0 or 1.
