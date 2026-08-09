# Read FreeSurfer ASCII format patch.

An ASCII format patch is a part of a brain surface mesh, and is a mesh
itself. It consists of vertices and faces. The ASCII patch format is
very similar to the ASCII surface format. **Note:** The contents of
ASCII and binary patch format files is different. The ASCII patch format
is not ideal for parsing, and loading such files is currently quite
slow.

## Usage

``` r
read.fs.patch.asc(filepath)
```

## Arguments

- filepath:

  string. Full path to the input patch file in ASCII patch format.

## Value

named list. The list has the following named entries: "vertices": see
return value of
[`read.fs.patch`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md).
"faces": numerical *n*x5 matrix. The columns are named, and appear in
the following order: 'face_index1': the one-based (R-style) face index.
'vert1_index1', 'vert2_index1', 'vert3_index1': integer vertex indices
of the face, they are one-based (R-style). 'face_index0': the zero-based
(C-style) face index.

## See also

Other patch functions:
[`fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.patch.md),
[`read.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md),
[`write.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.patch.md)
