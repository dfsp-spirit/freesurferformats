# Convert tris faces to quad faces by simple merging.

This is experimental. Note that it can only work if the number of
'tris_faces' is even, as two consecutive tris-faces will be merged into
one quad face. We could set the index to NA in that case, but I do not
know how FreeSurfer handles this, so we do not guess.

## Usage

``` r
faces.tris.to.quad(tris_faces)
```

## Arguments

- tris_faces:

  *nx3* integer matrix, the indices of the vertices making up the *n*
  tris faces.

## Value

n/2x4 integer matrix, the indices of the vertices making up the *n* quad
faces.

## Note

This function does not implement proper remeshing of tri-meshes to
quad-meshes. Use a proper mesh library if you need that.
