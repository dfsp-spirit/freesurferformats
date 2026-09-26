# Write b-values to a FSL-style bvals file.

Writes the diffusion weighting of each volume of a diffusion MRI dataset
to a plain text file, which is the *bvals* part of the FSL bvecs/bvals
format.

## Usage

``` r
write.dti.bval(filepath, bval, layout = "components")
```

## Arguments

- filepath:

  character string, path to the output file. The conventional extension
  is `.bval` or `.bvals`, but that is not enforced. Files ending in
  `.gz` are gzip-compressed.

- bval:

  numeric vector, the b-value of each volume, in s/mm^2.

- layout:

  character string, one of 'components' or 'volumes'. `components` (the
  default) writes all values into a single line, which is what the FSL
  tools produce, while `volumes` writes one value per line, which is
  what the Human Connectome Project distributes.

## Value

NULL, invisibly. Called for the side effect of writing the file.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
