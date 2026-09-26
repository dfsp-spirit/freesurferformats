# Write the volume info section of an LTA file.

The volume info section records the geometry of one of the two volumes
an LTA file relates: its dimensions, voxel sizes and the direction
vectors and center that describe its RAS space. The geometry is written
from a volume descriptor, and the direction vectors are the columns of
its voxel-to-RAS matrix while the center is the RAS coordinate of voxel
index `dim/2`, which is what FreeSurfer records there.

## Usage

``` r
lta.volume.info.lines(descriptor, section_name)
```

## Arguments

- descriptor:

  `NULL` or a volume descriptor, see
  [`volume.descriptor`](https://dfsp-spirit.github.io/freesurferformats/reference/volume.descriptor.md).

- section_name:

  character string, either 'src' or 'dst'.

## Value

character vector, the lines of the section.
