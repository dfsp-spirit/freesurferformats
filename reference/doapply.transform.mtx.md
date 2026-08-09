# Apply a spatial transformation matrix to the given coordinates.

Apply a spatial transformation matrix to the given coordinates.

## Usage

``` r
doapply.transform.mtx(coords, mtx, as_mat = FALSE)
```

## Arguments

- coords:

  nx3 (cartesian) or nx4 (homogeneous) numerical matrix, the input
  coordinates. If nx4, left as is for homogeneous notation, if nx3
  (cartesian) a 1 will be appended as the 4th position.

- mtx:

  a 4x4 numerical transformation matrix

- as_mat:

  logical, whether to force the output coords into a matrix (even if the
  input was a vector/a single coordinate triple).

## Value

the coords after applying the transformation. If coords was nx3, nx3 is
returned, otherwise nx4.

## Examples

``` r
coords_tf <- doapply.transform.mtx(c(1.0, 1.0, 1.0), mni152reg())
coords_tf
#> [1] 0.9649 2.5627 2.1588
doapply.transform.mtx(coords_tf, solve(mni152reg()))
#> [1] 1 1 1
```
