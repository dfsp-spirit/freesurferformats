# Constructor for fs.patch

Constructor for fs.patch

## Usage

``` r
fs.patch(vertices, faces = NULL)
```

## Arguments

- vertices:

  numerical *n*x5 matrix (or *n*x7 matrix), see
  [`read.fs.patch`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md)
  for details. If it has 5 columns, columns 6-7 will be computed
  automatically from the first 5 columns (from column 1 and 5).

- faces:

  numerical *n*x5 matrix, see
  [`read.fs.patch.asc`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.asc.md)
  for details. Can be `NULL`.

## Value

instance of class `fs.patch`

## See also

Other patch functions:
[`read.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.md),
[`read.fs.patch.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.patch.asc.md),
[`write.fs.patch()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.patch.md)

## Examples

``` r
num_vertices <- 6L
# a tiny patch
vertices <- matrix(rep(0., num_vertices * 5), ncol = 5)
vertices[, 1] <- seq.int(num_vertices)
# 1-based vertex indices
vertices[, 2:4] <- matrix(rnorm(num_vertices * 3, 8, 2), ncol = 3)
# vertex coords
vertices[, 5] <- rep(0L, num_vertices)
# is_border
vertices[3, 5] <- 1L
# set a vertex to be a border vertex
patch <- fs.patch(vertices)
patch
#> Brain surface patch with 6 vertices, 1 on patch border.
#> Patch contains no face information.
```
