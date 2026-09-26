# Read b-vectors from a FSL-style bvecs file.

Reads the diffusion gradient directions of a diffusion MRI dataset, i.e.
the *b-vectors* part of the FSL bvecs/bvals format. The file contains
unit vectors (or zero vectors for the *b=0* volumes), stored as three
rows of *n* values, one row per component, one column per volume.

## Usage

``` r
read.dti.bvec(filepath, layout = "auto")
```

## Arguments

- filepath:

  character string, path to the b-vectors file, typically ending in
  `.bvec` or `.bvecs`, but without an extension in the HCP layout.

- layout:

  character string, one of 'auto', 'components' or 'volumes'.
  `components` means that there are 3 lines, one per vector component
  (the FSL layout, and what the file name `bvecs` implies), `volumes`
  means one volume per line, i.e. *n* lines with 3 values each (the HCP
  layout). With 'auto' (the default), the layout is derived from the
  matrix dimensions and a warning is raised for a square table, which is
  ambiguous.

## Value

a numeric matrix with *n* rows and 3 columns, one row per volume, the
columns being the x, y and z component of the gradient direction. Note
that these vectors are given with respect to the *image* axes, not to
the scanner axes, so they are only meaningful together with the image
they belong to: converting them to scanner space requires the transform
of that image. Use
[`read.dti.gradients`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md)
to read b-vectors and b-values together, with consistency checks.

## Note

The b-vectors are returned exactly as they are stored, i.e. they are
*not* renormalized, and missing values (see
[`read.dti.gradients`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md))
are not replaced here.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.grad.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
