# Read an ANALYZE 7.5 or NIFTI v1 pair file as an fs.volume.

Read a volume from the two-file image formats ANALYZE 7.5 and NIFTI v1:
a 348 byte header (`<base>.hdr`) and the voxel data in a separate file
(`<base>.img`). Read
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md)
for the difference between the two variants and for the limitations of
the ANALYZE header.

## Usage

``` r
read.fs.volume.analyze(
  filepath,
  flatten = FALSE,
  with_header = FALSE,
  drop_empty_dims = FALSE,
  spm = FALSE
)
```

## Arguments

- filepath:

  character string, the path to one of the two files of the pair, or to
  its base name. See
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md).

- flatten:

  logical. Whether to flatten the return volume to a 1D vector. Useful
  if you know that this file contains 1D morphometry data.

- with_header:

  logical. Whether to return the header as well. If TRUE, return an
  instance of class `fs.volume` for data with at least 3 dimensions, a
  named list with entries "data" and "header". The latter is another
  named list which contains the header data. These header entries exist:
  "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT;
  4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions.
  E.g., c(256, 256, 256, 1). These header entries may exist:
  "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists
  if "has_mr_params" is 1). See the `mghheader.*` functions, like
  [`mghheader.vox2ras.tkreg`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.tkreg.md),
  to compute more information from the header fields.

- drop_empty_dims:

  logical, whether to drop empty dimensions of the returned data

- spm:

  logical, whether to interpret the ANALYZE header the way SPM,
  FreeSurfer and the other tools of the ecosystem do. This concerns the
  fields that ANALYZE 7.5 leaves undefined but that SPM uses: `funused1`
  is the scale factor of the data, and `originator` holds the voxel
  coordinates of the image origin. It makes this function apply the
  scale factor, and use the origin to derive a transformation matrix
  when the file has no MATLAB sidecar file (see the geometry section
  below). Only relevant for ANALYZE files, since the NIFTI v1 variant of
  the pair stores a proper transformation matrix.

## Value

an `fs.volume` instance, i.e. a named list with the entries `data` (the
voxel data) and `header` (the image header), or only the data array if
`with_header` is `FALSE`. The header is the format specific header (an
ANALYZE 7.5 header as returned by
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md)
for ANALYZE files, a NIFTI v1 header as returned by
[`read.nifti1.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti1.header.md)
for NIFTI pair files), and not an MGH header. In addition, the following
entries are present:

- `vox2ras_matrix`:

  the 4x4 voxel-to-RAS transformation matrix, computed from the
  `sform`/`qform` header fields for NIFTI v1 pair files. For ANALYZE
  files this entry is only present if `spm` is `TRUE`, see the note
  below.

- `vox2ras_source`:

  character string, a description of where the matrix comes from, one of
  `'sform'`, `'qform'` (for NIFTI v1 pair files), `'mat sidecar'`,
  `'spm origin'` or `'analyze convention'` (for ANALYZE files, see the
  geometry section). Only present if `vox2ras_matrix` is present.

- `voldim`:

  only present if `flatten` is `TRUE`, exactly as in the other volume
  readers.

- `filepath`, `filepath_header`, `filepath_image`:

  the paths of the header and data files.

## The geometry of ANALYZE 7.5 files

The ANALYZE 7.5 header stores the voxel sizes (`pix_dim`) but neither
the direction of the voxel axes in world space nor the position of the
image in it. This means that the left/right orientation of an ANALYZE
image is *not known* from the header, and no implementation of the
format can know it: the format is famous for exactly this problem, and
the `orient` field that was added to fix it is set by some tools and
ignored by others.

There are two ways to get a transformation matrix for such a file, and
this function supports both:

- The MATLAB sidecar file:

  SPM and FreeSurfer write the transformation matrix into a file named
  `<base>.mat` next to the `<base>.img` file, as a MATLAB v4 file with a
  variable named `mat` or `M`. This matrix is real information from the
  file set, not a convention, so it is used whenever such a file is
  present, and the `vox2ras_source` of the result is `'mat sidecar'`.
  The `M` variant does not contain the flip of the first voxel axis that
  the format's original software applies, so that flip is added (this is
  also what the reference implementation of the format, nibabel, does).
  Note that the matrix in the file maps the *1-based* voxel indices that
  MATLAB uses, and that the matrix returned here maps the 0-based
  indices that R and this package use: the difference is the sum of the
  rows of the rotation part, i.e. up to several millimeters, and getting
  it wrong is exactly the kind of silent error that is hard to notice.
  FreeSurfer writes only the `M` variable, nibabel writes both. MATLAB
  files of version 5 and newer (the `.mat` files that recent MATLAB
  versions write, which are compressed) cannot be read; such a file is
  reported and ignored, the volume data is still returned.

- The SPM convention (`spm = TRUE`):

  If there is no sidecar file, the SPM and FreeSurfer tools derive the
  geometry from the fields that ANALYZE leaves unused: the `originator`
  field holds the voxel coordinates of the image origin, the voxel sizes
  give the axis lengths, and the axes are assumed to point to the left,
  the front and the top (`diag(-x, y, z)`, the convention of the
  format's original software). The same `spm = TRUE` applies the scale
  factor that SPM stores in `funused1`. Both are heuristics that the
  file does not state, which is why they are not used by default: a
  matrix from a convention can be wrong by a mirror image.

Without a sidecar file and with `spm = FALSE` (the default), no matrix
is reported at all: the voxel sizes and the orientation code are
returned as they are stored, and you can build a matrix from them if the
convention of your choice is known to be the right one for your data.

## See also

[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md),
[`read.analyze.data`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.data.md),
[`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)

## Examples

``` r
hdrfile <- system.file("extdata", "analyze", "tiny_u8.hdr",
  package = "freesurferformats", mustWork = TRUE
)
vol <- read.fs.volume.analyze(hdrfile, with_header = TRUE)
dim(vol$data)
#> [1] 4 3 2
vol$header$pix_dim
#> [1] 1 1 2 3 1 1 1 1
```
