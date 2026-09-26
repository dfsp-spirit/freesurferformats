# Write b-vectors to a FSL-style bvecs file.

Writes the diffusion gradient directions of a diffusion MRI dataset to a
plain text file, which is the *bvecs* part of the FSL bvecs/bvals
format. Note that these vectors are interpreted relative to the *image*
axes, so the file is only meaningful together with the image it was
derived from.

## Usage

``` r
write.dti.bvec(filepath, bvec, layout = "components")
```

## Arguments

- filepath:

  character string, path to the output file. The conventional extension
  is `.bvec` or `.bvecs`, but that is not enforced. Files ending in
  `.gz` are gzip-compressed.

- bvec:

  numeric matrix with 3 columns (one row per volume) or 3 rows (one
  column per volume), the gradient direction of each volume.

- layout:

  character string, one of 'components' or 'volumes'. `components` (the
  default) writes the file with 3 lines, one per vector component, which
  is the FSL layout, while `volumes` writes one volume per line, which
  is what the Human Connectome Project distributes.

## Value

NULL, invisibly. Called for the side effect of writing the file.

## Note

A square 3x3 input is ambiguous. Since a matrix passed in memory follows
the R convention of one row per volume, the rows are written as the
volumes here;
[`read.dti.bvec`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md)
assumes the components for a square file, so reading such a file back
yields the transposed matrix and warns about the ambiguity.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
