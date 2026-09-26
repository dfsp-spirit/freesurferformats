# Validate and normalize a diffusion MRI gradient table.

Checks that a set of b-vectors and b-values is consistent, replaces
missing values by `b=0` volumes, and reports suspicious entries. This is
the validation used by
[`read.dti.gradients`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md),
but it can also be called directly on gradients from any other source.

## Usage

``` r
validate.dti.gradients(bvec, bval, n_volumes = NULL)
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

## Value

a named list with the entries `bvec` (numeric matrix with *n* rows and 3
columns, one row per volume) and `bval` (numeric vector of length *n*).

## Note

The rules for missing values match those of MRtrix3: a missing b-value
in a volume with a valid direction, or a missing direction in a volume
with a non-zero b-value, is an error, because such a volume cannot be
interpreted. Everything else is treated as a `b=0` volume. The norm of
the gradient vectors is never changed, and a b-value is never rescaled,
because that would silently alter the data.
