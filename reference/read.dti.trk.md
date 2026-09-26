# Read fiber tracks from Diffusion Toolkit in trk format.

Read fiber tracks from Diffusion Toolkit in trk format.

## Usage

``` r
read.dti.trk(
  filepath,
  shift_origin = TRUE,
  max_tracks = Inf,
  skip_tracks = 0L,
  bbox = NULL,
  coords = NULL
)
```

## Arguments

- filepath:

  character string, path to file in trk format. Gzip-compressed files
  are supported as well (the compression is detected from the file
  content, so a `.trk.gz` file is read like any other TRK file), which
  is convenient since tractograms are large and are regularly stored
  compressed. Note that track files cannot be compressed on the fly for
  other software: the TrackVis tools and MRtrix do not read compressed
  track files.

- shift_origin:

  logical, whether to apply the half-voxel origin shift when computing
  the corrected vox2ras matrix. The TRK format stores a matrix that maps
  to the voxel corner, not the voxel center (as is the NIfTI
  convention). Set to `TRUE` (the default) to compute the corrected
  `vox2ras` that maps to voxel centers, as used by TrackVis. Set to
  `FALSE` if the file was written by DSI Studio, which does not apply
  this shift. See the notes for details.

- max_tracks:

  numeric, the maximum number of tracks to read. Use `Inf` (the default)
  to read all tracks. This allows reading a subset of a very large
  tractogram without holding all of it in memory.

- skip_tracks:

  integer, the number of tracks to skip before reading any. Skipped
  tracks are never held in memory. Note that the count refers to the
  tracks that a reader returns, and that empty tracks (tracks without
  any point, which the TRK format allows) are returned as well, e.g., a
  file with one regular track, then an empty one, yields the empty track
  for `skip_tracks = 1`. The only exception is a `bbox` filter, which
  drops empty tracks, since no point of them can be inside the box.

- bbox:

  numeric vector of length 6 or NULL. If given, only tracks that have at
  least one point inside the box are read, the box is given as
  `c(xmin, xmax, ymin, ymax, zmin, zmax)`.

- coords:

  character string or NULL, the coordinate system of the returned track
  coordinates. One of 'native' (the coordinates exactly as stored in the
  file, which for TRK files is usually 'voxelmm' space, in which the
  coordinates refer to voxel corners) or 'ras' (RAS+ mm space in which a
  coordinate of (0,0,0) is the center of the first voxel, which is what
  'nibabel' and DIPY return by default). If `NULL` (the default), the
  coordinates are returned as stored, but a warning is raised when the
  file stores a transformation that is not the identity, since the
  coordinates are then not in RAS space and plotting them directly would
  produce e.g., a mirrored brain. Pass `'native'` to silence that
  warning.

  The `bbox` filter is always interpreted in the same coordinate system
  as the returned coordinates.

## Value

named list, the parsed file data. The naming of the variables follows
the spec at `http://trackvis.org/docs/?subsect=fileformat`. The returned
header will contain the field `vox2ras` (the raw matrix stored in the
TRK file, mapping from mm space to RAS) and, if `shift_origin` is
`TRUE`, the additional field `vox2ras_corrected` (the computed matrix
mapping from voxel indices to voxel center RAS coordinates). It also
contains the entry `coords_space` ('native' or 'ras'), which records the
coordinate system the returned tracks are in, so that the result can be
passed to
[`write.dti.trk()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.trk.md)
without losing that information. The tracks are stored in an
[`is.fs.tracts`](https://dfsp-spirit.github.io/freesurferformats/reference/is.fs.tracts.md)
instance: `tracks[[i]]` returns a list with the entries `coords` (n x 3
matrix), `num_points` (integer), `scalars` (matrix or NULL) and
`properties` (numeric vector or NULL) for the i-th track. Use
`fs.tracts.lengths(trk$tracks)` to get the number of points of each
track, and `as.list(trk$tracks)` to convert to a plain list.

## Note

The 4x4 matrix stored in TRK files (labeled `vox_to_ras` in the spec) is
actually a transformation from **mm** space to RAS, not from voxel space
to RAS. The TRK format was designed by TrackVis with the assumption that
voxels are 1 mm\\^3\\, and that coordinates refer to voxel corners
rather than centers. To obtain the actual vox2ras matrix (voxel center
in RAS), the raw matrix must be combined with a voxel-size scaling and a
half-voxel offset correction:
`vox2ras_corrected = mm2ras %*% mm_correction %*% vox2mm`, where
`vox2mm` scales by the inverse voxel size and `mm_correction` shifts by
-0.5 mm. Note that DSI Studio does **not** apply this half-voxel shift,
so you may need to set `shift_origin=FALSE` for DSI Studio files.

Use `coords = 'ras'` to have the transformation applied to the returned
coordinates. That transformation additionally accounts for the
`voxel_order` stored in the header, like the reference implementation in
'nibabel' does, see
[`trackvis.affine.to.rasmm`](https://dfsp-spirit.github.io/freesurferformats/reference/trackvis.affine.to.rasmm.md).
Note that the matrix stored in the header alone is **not** sufficient to
transform the coordinates: it has to be combined with the voxel sizes,
the half-voxel offset and the orientation.

## Examples

``` r
if (FALSE) { # \dontrun{
trk <- read.dti.trk("~/simple.trk")
trk2 <- read.dti.trk("~/standard.trk")
trk3 <- read.dti.trk("~/complex_big_endian.trk")

# Coordinates in RAS+ mm, ready for plotting against an MNI template:
trk_ras <- read.dti.trk("~/simple.trk", coords = "ras")
} # }
```
