# Write tracks to a file in TrackVis TRK format.

Writes streamlines in the TRK format described at
`http://trackvis.org/docs/?subsect=fileformat`. The output is read by
TrackVis, by DSI Studio and by
[`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md).

## Usage

``` r
write.dti.trk(
  tracts,
  filepath,
  header = NULL,
  coords_space = NULL,
  endian = "little",
  gzip = NULL
)
```

## Arguments

- tracts:

  the tracks to write, either an `fs.tracts` instance as returned in the
  `tracks` entry of
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md),
  or a list of numeric matrices with 3 columns, or a single such matrix.
  Per-point scalars and per-track properties are written if the input is
  an `fs.tracts` instance that has them.

- filepath:

  character string, the path of the file to write.

- header:

  named list or NULL, the header of the file the tracks were read from,
  whose metadata (voxel sizes, dimensions, the voxel-to-RAS matrix, the
  voxel order, and the scalars and properties of the file) is reused. If
  `NULL`, a minimal header is written that describes coordinates in 1 mm
  isotropic voxels with an identity voxel-to-RAS matrix, which is the
  case that needs no coordinate correction at all.

- coords_space:

  character string or NULL, the coordinate system the tracks are in,
  either 'native' (the coordinates are written as they are) or 'ras'
  (the coordinates are RAS+ mm and are transformed back to the space
  used in the file, which is what 'nibabel' does when saving). If `NULL`
  (the default), the value stored in `header$coords_space` is used when
  present, and 'native' otherwise. This makes a read-write round trip
  with `coords = 'ras'` work without any further arguments.

- endian:

  character string, 'little' (the default) or 'big'. TrackVis writes
  little endian files, big endian support is for reading files written
  on old big endian systems.

- gzip:

  logical or NULL, whether to gzip-compress the output. If `NULL` (the
  default), the file is compressed when the file name ends in '.gz'.
  Note that the TrackVis tools and MRtrix do not read compressed track
  files, so this is useful for archiving and for passing the file back
  to
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md),
  but not for exchanging it with other software.

## Value

the file path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
trk <- read.dti.trk("brain.trk");
write.dti.trk(trk$tracks, "copy.trk", header = trk$header);

# Write RAS coordinates back to a file that uses voxelmm space:
trk_ras <- read.dti.trk("brain.trk", coords = "ras");
write.dti.trk(trk_ras$tracks, "copy.trk", header = trk_ras$header, coords_space = "ras");
} # }
```
