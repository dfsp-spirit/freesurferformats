# Read FreeSurfer binary or ASCII patch file.

A patch is a subset of a surface. Note that the contents of ASCII and
binary patch format files is different. A binary format patch contains
vertices only, without connection (face) information. ASCII patch files
can also contain face data. See the return value description for
details.

## Usage

``` r
read.fs.patch(filepath, format = "auto")
```

## Arguments

- filepath:

  string. Full path to the input patch file. An example file is
  `FREESURFER_HOME/subjects/fsaverage/surf/lh.cortex.patch.3d`.

- format:

  one of 'auto', 'asc', or 'bin'. The format to assume. If set to 'auto'
  (the default), binary format will be used unless the filepath ends
  with '.asc'.

## Value

named list with 2 entries: "faces": can be NULL, only available if the
format is ASCII, see return value of
[`read.fs.patch.asc`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.asc.md).
"vertices": numerical *n*x7 matrix. The columns are named, and appear in
the following order: 'vert_index1': the one-based (R-style) vertex
index. 'x', 'y', 'z': float vertex coordinates. 'is_border': integer, 1
if the vertex lies on the patch border, 0 otherwise (treat as logical).
'raw_vtx': integer, the raw vtx value encoding index and border.
'vert_index0': the zero-based (C-style) vertex index.

## See also

Other patch functions:
[`fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.patch.md),
[`read.fs.patch.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.asc.md),
[`write.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.patch.md)
