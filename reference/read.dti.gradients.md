# Read and validate a diffusion MRI gradient table.

The main entry point for reading diffusion gradients. It reads b-vectors
and b-values from either a pair of FSL-style files or from a single
MRtrix-style gradient table file, checks that they are consistent, and
reports suspicious entries. Use this rather than calling
[`read.dti.bvec`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md)
and
[`read.dti.bval`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md)
separately if you intend to use the gradients for anything, because a
mismatched or malformed gradient table is silently wrong otherwise.

## Usage

``` r
read.dti.gradients(bvec, bval = NULL, n_volumes = NULL, layout = "auto")
```

## Arguments

- bvec:

  character string (path to a b-vectors file), or a numeric matrix with
  one row per volume (or one column per volume) and 3 columns.

- bval:

  character string (path to a b-values file), or a numeric vector with
  one value per volume. Can be `NULL` if `bvec` identifies both files.

- n_volumes:

  scalar numeric or integer, the number of volumes in the DWI image that
  the gradients belong to, used to check that the table matches the
  image. Typically `dim(volume$data)[4]`. Set to `NULL` (the default) to
  skip this check.

- layout:

  character string, passed on to the readers, one of 'auto',
  'components' or 'volumes'.

## Value

a named list with the entries `bvec` (numeric matrix with *n* rows and 3
columns, one row per volume, components in *image* space) and `bval`
(numeric vector of length *n*).

## Details

Both parameters accept either a file path or already read data, so the
function can also be used to validate gradients that were obtained from
somewhere else. If `bval` is `NULL` and `bvec` is a single file path,
the file is interpreted as follows. A file whose name contains `bvec` is
read as a b-vectors file (since the name is the user's clearest
statement about the content, and a b-vectors file with 4 volumes would
otherwise look exactly like a 3-volume gradient table). Otherwise, a
file that has 4 values per line, or 4 lines, is read as an MRtrix
gradient table. Otherwise the file is read as b-vectors and a matching
b-values file is looked up next to it, which covers the common dataset
layouts such as BIDS (`sub-01_dwi.bvec` with `sub-01_dwi.bval`).

## Note

The following problematic cases are reported, but the data is always
returned as read, except for the missing values, which are replaced by
zeros: (1) `NA`/`NaN` entries, which are interpreted as `b=0` volumes,
as MRtrix3 does, provided that they do not leave a volume with a b-value
but no direction or vice versa, which is an error; (2) gradient vectors
whose norm deviates from 1, which is reported because MRtrix3 rescales
the b-value by the squared norm in this situation; (3) volumes with a
b-value above the b=0 threshold but a zero direction; (4) volumes with a
small positive b-value but a non-zero direction.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
