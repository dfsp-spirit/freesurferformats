# Determine the frame of the world space of a transformation.

The world space that a transformation between voxel coordinates refers
to depends on the format it was read from: an FSL matrix uses the FSL
convention, see
[`fsl.scaled.voxel.matrix`](https://dfsp-spirit.github.io/freesurferformats/reference/fsl.scaled.voxel.matrix.md),
while the matrices of the other formats use the RAS space of the volume
headers. The frame is taken from the volume descriptors if they state it
and is derived from the format otherwise, so that
[`transform2world`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2world.md)
and
[`transform2voxel`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md)
always agree.

## Usage

``` r
# S3 method for class 'world.frame'
transform(tf)
```

## Arguments

- tf:

  an `fs.transform` instance.

## Value

character string, either 'fsl' or 'scanner'.
