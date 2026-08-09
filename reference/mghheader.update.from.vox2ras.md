# Update mghheader fields from vox2ras matrix.

Update mghheader fields from vox2ras matrix.

## Usage

``` r
mghheader.update.from.vox2ras(header, vox2ras)
```

## Arguments

- header:

  Header of the mgh datastructure, as returned by
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).

- vox2ras:

  4x4 numerical matrix, the vox2ras transformation matrix.

## Value

a named list representing the header
