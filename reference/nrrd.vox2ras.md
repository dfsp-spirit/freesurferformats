# Compute the voxel-to-RAS matrix of a NRRD volume.

NRRD stores the geometry of a volume as one direction vector per space
axis ('space directions'), the world position of the center of voxel
(0,0,0) ('space origin') and the world space these refer to ('space').
Voxel indices are 0-based. The matrix returned here always maps to RAS+
coordinates (x = right, y = anterior, z = superior), which is the space
used by FreeSurfer, by the NIfTI standard and by the rest of this
package: a file that states the LPS space therefore has the sign of its
first two axes flipped.

## Usage

``` r
nrrd.vox2ras(fields, dimension, filepath = "")
```

## Arguments

- fields:

  named list, the parsed header fields.

- dimension:

  integer, the number of dimensions of the volume.

- filepath:

  character string, path to the file, used in warnings.

## Value

named list with entries `matrix` (the 4x4 voxel-to-RAS matrix, or NULL
when the file carries no space information at all) and `source`
(character string, how the matrix was derived: 'space directions',
'spacings' or NULL).
