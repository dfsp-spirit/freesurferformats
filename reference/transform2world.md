# Convert a transformation to the world (RAS) coordinate space.

Transformation files often store their matrix in voxel coordinates,
which means that the matrix alone cannot be used to transform world
coordinates (e.g. the coordinates of a brain surface vertex, or a peak
coordinate from another study): the geometry of the volumes the matrix
relates is required as well. This function converts such a
transformation into one that operates on world coordinates.

The result depends on the format the transformation was read from,
because the formats disagree about their world space, which is why this
is not a pure matrix operation:

- For an FSL matrix ('fslmat'), the world space is the one FSL uses:
  unit voxel axes with a flipped first axis, see
  [`fsl.scaled.voxel.matrix`](https://dfsp-spirit.github.io/freesurferformats/reference/fsl.scaled.voxel.matrix.md).
  Both MRtrix3 and FreeSurfer implement exactly this, and the result is
  a transformation between FSL world coordinates. The `src` and `dst`
  entries of the result have the frame 'fsl'.

- For an LTA of type 0 (VOX2VOX), the world space is the RAS space of
  the two volume geometries: the result is
  `vox2ras_dst \%*\% matrix \%*\% solve(vox2ras_src)`. The descriptors
  have the frame 'scanner'.

## Usage

``` r
transform2world(tf, src = NULL, dst = NULL)
```

## Arguments

- tf:

  an `fs.transform` instance with a matrix in voxel coordinates
  (`space_in` and `space_out` are 'voxel'). A transformation that
  already operates on world coordinates is returned unchanged.

- src:

  `NULL` or the volume the transformation maps from (the `-in` image of
  FSL, the `src` volume of an LTA). Required for FSL matrices, since
  their files do not record the volumes.

- dst:

  `NULL` or the volume the transformation maps to (the `-ref` image of
  FSL, the `dst` volume of an LTA). Required for FSL matrices.

## Value

an `fs.transform` instance whose matrix operates on world coordinates.

## See also

Other header coordinate space:
[`invert.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/invert.fs.transform.md),
[`is.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/is.fs.transform.md),
[`mghheader.is.ras.valid()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.is.ras.valid.md),
[`mghheader.ras2vox()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.md),
[`mghheader.ras2vox.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.ras2vox.tkreg.md),
[`mghheader.scanner2tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.scanner2tkreg.md),
[`mghheader.tkreg2scanner()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.tkreg2scanner.md),
[`mghheader.vox2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.md),
[`mghheader.vox2ras.tkreg()`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
[`print.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.transform.md),
[`read.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.md),
[`read.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.dat.md),
[`read.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.fslmat.md),
[`read.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.itk.md),
[`read.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.lta.md),
[`read.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.xfm.md),
[`sm0to1()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm0to1.md),
[`sm1to0()`](https://dfsp-spirit.github.io/freesurferformats/reference/sm1to0.md),
[`summary.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/summary.fs.transform.md),
[`transform2lps()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2lps.md),
[`transform2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2ras.md),
[`transform2voxel()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md),
[`write.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.md),
[`write.fs.transform.dat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.dat.md),
[`write.fs.transform.fslmat()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.fslmat.md),
[`write.fs.transform.itk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.itk.md),
[`write.fs.transform.lta()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.lta.md),
[`write.fs.transform.xfm()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.transform.xfm.md)

## Examples

``` r
# Read an LTA file, which records both volumes it relates, and convert it to world coordinates.
lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
tf <- read.fs.transform(lta_file)
tf_world <- transform2world(tf)
tf_world$space_in
#> [1] "ras"
tf_world$matrix
#>              [,1]        [,2]        [,3]         [,4]
#> [1,]  1.054775715  0.05151368 0.007081697   0.06612823
#> [2,] -0.060359493  0.85348159 0.274788439 -20.39745541
#> [3,]  0.005556736 -0.33849043 1.116058946  11.51445715
#> [4,]  0.000000000  0.00000000 0.000000000   1.00000000
```
