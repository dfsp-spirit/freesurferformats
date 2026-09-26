# Read a gradient table in MRtrix3 format.

Reads a diffusion gradient table in MRtrix format: a plain text file
with one line per DWI volume, each line holding the four space-separated
values `[ x y z b ]`, with the direction in *scanner* space and the
b-value in s/mm^2. This is the format expected by the `-grad` option of
the MRtrix3 commands (and produced by `-export_grad_mrtrix`), and it is
also the format in which MRtrix3 stores the table in the `dw_scheme`
entry of a `.mif` header. A first line consisting of a single integer is
accepted as a volume count header.

## Usage

``` r
read.dti.grad(filepath, layout = "auto")
```

## Arguments

- filepath:

  character string, path to the file. There is no fixed file name
  convention, `grad.b` and `encoding.b` are common. Gzip-compressed
  files are supported.

- layout:

  character string, one of 'auto', 'components' or 'volumes'. `volumes`
  means one volume per line (4 values per line, the MRtrix layout),
  `components` the transposed variant (4 lines). With 'auto' (the
  default), the layout is derived from the matrix dimensions.

## Value

a numeric matrix with *n* rows and 4 columns named 'x', 'y', 'z' and
'b', one row per volume. The directions are in scanner space.

## See also

Other dti functions:
[`read.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bval.md),
[`read.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.bvec.md),
[`read.dti.gradients()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
[`write.dti.bval()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bval.md),
[`write.dti.bvec()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.bvec.md),
[`write.dti.grad()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.grad.md)
