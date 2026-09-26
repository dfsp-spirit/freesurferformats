# Create an fs.transform instance.

An `fs.transform` is a linear transformation matrix together with the
coordinate spaces that it maps between. It is the common representation
that all transformation file format readers and writers in this package
return and accept, so that a matrix read from a file always states what
its numbers mean: a bare 4x4 matrix does not tell whether it maps voxel
indices or world coordinates, in which direction it goes, or whether the
voxel indices are zero- or one-based.

The `matrix` field always maps from the source to the target, i.e.,
applying it to a coordinate means
`target_coord = matrix \%*\% c(source_coord, 1)`, which is the same
convention that
[`doapply.transform.mtx`](https://dfsp-spirit.github.io/freesurferformats/reference/doapply.transform.mtx.md)
uses. The inverse of the transformation is never stored, see
[`invert.fs.transform`](https://dfsp-spirit.github.io/freesurferformats/reference/invert.fs.transform.md).

## Usage

``` r
fs.transform(
  matrix,
  space_in = NA_character_,
  space_out = NA_character_,
  voxel_base = NA_integer_,
  src = NULL,
  dst = NULL,
  format = NA_character_,
  source = NULL,
  type = NULL,
  ...
)
```

## Arguments

- matrix:

  4x4 numerical matrix, the transformation matrix. Required.

- space_in:

  character string, the kind of coordinates the matrix maps from, one of
  'voxel' (voxel indices of a volume), 'ras' (world coordinates,
  right-anterior-superior in millimeters) or 'lps' (world coordinates,
  left-posterior-superior in millimeters, used by ITK and ITK-based
  tools like ANTs). Use `NA` if the file did not state the space.

- space_out:

  character string, the kind of coordinates the matrix maps to, see
  `space_in`.

- voxel_base:

  integer, either 0 or 1. The index of the first voxel in the
  coordinates the matrix consumes and produces, i.e. 1 for the
  convention used by FreeSurfer and tkregister, and 0 for the one used
  by NIfTI, FSL and ITK. Must be `NA` unless `space_in` or `space_out`
  is 'voxel', since the value is meaningless for world coordinates.

- src:

  `NULL` or named list, the volume or template that the matrix maps
  from. See `volume.descriptor`.

- dst:

  `NULL` or named list, the volume or template that the matrix maps to.

- format:

  character string, the file format the transform was read from, one of
  'lta', 'dat', 'xfm' or 'fslmat'. Use `NA` if the transform was not
  read from a file.

- source:

  `NULL` or character string, the path of the file the transform was
  read from.

- type:

  `NULL` or character string, the transform type as stated by the file
  format (e.g. 'Linear' for an xfm file, or the numeric LTA type as a
  string). This is format-specific metadata and is not interpreted by
  the package.

- ...:

  additional named fields to store in the transform, e.g. the parsed
  header of the file it was read from. They are not validated and are
  preserved for formats that carry extra metadata.

## Value

an `fs.transform` instance.
