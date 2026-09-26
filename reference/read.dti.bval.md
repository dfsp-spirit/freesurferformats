# Read b-values from a FSL-style bvals file.

Reads the b-values of a diffusion MRI dataset, i.e. the diffusion
weighting of each volume in units of s/mm^2. This implements the *bvals*
part of the FSL format: a plain text file with one value per volume,
usually in a single row. The HCP variant, which stores one value per
line, is read as well.

## Usage

``` r
read.dti.bval(filepath, layout = "auto")
```

## Arguments

- filepath:

  character string, path to the b-values file, typically ending in
  `.bval` or `.bvals`, but without an extension in the HCP layout.

- layout:

  character string, one of 'auto', 'components' or 'volumes'.
  `components` means that the file stores the values in a single line
  (the FSL layout), `volumes` means one value per line (the HCP layout).
  With 'auto' (the default), the layout is derived from the file
  content.

## Value

numeric vector of length *n*, the b-value of each of the *n* volumes.

## See also

Other dti functions:
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
