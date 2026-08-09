# Write a surface patch

Write a surface patch, i.e. a set of vertices and patch border
information, to a binary patch file.

## Usage

``` r
write.fs.patch(filepath, patch)
```

## Arguments

- filepath:

  string. Full path to the output patch file. If it ends with ".gz", the
  file is written in gzipped format. Note that this is not common, and
  that other software may not handle this transparently.

- patch:

  an instance of class `fs.patch`, see
  [`read.fs.patch`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md).

## Value

the patch, invisible

## See also

Other patch functions:
[`fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.patch.md),
[`read.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md),
[`read.fs.patch.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.asc.md)
