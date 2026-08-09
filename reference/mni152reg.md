# Get fsaverage (MNI305) to MNI152 transformation matrix.

The uses the 4x4 matrix from the FreeSurfer CoordinateSystems
documentation.

## Usage

``` r
mni152reg()
```

## Note

There are better ways to achieve this transformation than using this
matrix, see Wu et al., 'Accurate nonlinear mapping between MNI
volumetric and FreeSurfer surface coordinate system', Hum Brain Mapp.
2018 Sep; 39(9): 3793–3808. doi: 10.1002/hbm.24213. The mentioned method
is available in R from the 'regfusionr' package (GitHub only atom, not
on CRAN).

## Examples

``` r
coords_tf <- doapply.transform.mtx(c(10.0, -20.0, 35.0), mni152reg())
coords_tf
#> [1]  10.6941 -18.4064  36.1385
#  10.695, -18.409, 36.137
doapply.transform.mtx(coords_tf, solve(mni152reg()))
#> [1]  10 -20  35
```
