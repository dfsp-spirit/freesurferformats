# Write a gradient table in MRtrix3 format.

Writes a diffusion gradient table to a plain text file with one line per
DWI volume, each line holding the four space-separated values
`[ x y z b ]`. This is the format that the `-grad` option of the MRtrix3
commands expects, and the format of the `dw_scheme` entry in the header
of a `.mif` image.

## Usage

``` r
write.dti.grad(filepath, bvec, bval = NULL, layout = "volumes")
```

## Arguments

- filepath:

  character string, path to the output file. Files ending in `.gz` are
  gzip-compressed.

- bvec:

  numeric matrix with 3 columns (one row per volume) or 3 rows (one
  column per volume), the gradient directions. Alternatively a full
  gradient table, i.e. a numeric matrix with 4 columns named or ordered
  'x', 'y', 'z' and 'b', as returned by
  [`read.dti.grad`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md).
  If a full table is given, the `bval` parameter must be left at `NULL`.

- bval:

  numeric vector, the b-value of each volume, in s/mm^2. Ignored if
  `bvec` is a full gradient table.

- layout:

  character string, one of 'volumes' or 'components'. `volumes` (the
  default) writes one volume per line, which is the MRtrix layout, while
  `components` writes the transposed variant with 4 lines.

## Value

NULL, invisibly. Called for the side effect of writing the file.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md)
