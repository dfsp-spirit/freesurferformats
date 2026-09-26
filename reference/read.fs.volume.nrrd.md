# Read volume data from a file in NRRD format.

Reads a volume from a file in NRRD format (`.nrrd`), i.e. the format
that 3D Slicer, ITK/SimpleITK, DTI-TK, DSI Studio and the `teem` tools
use. Detached headers (`.nhdr`, whose data are in separate files) and
gzip-compressed files (`.nrrd.gz`) are supported as well, and the reader
is implemented in this package, so no additional R package or external
software is needed. The values are returned in the order in which they
are stored in the file, and NRRD stores the first axis fastest, which is
the order that R uses for arrays as well, so the returned array is
shaped exactly like the volume described by the header.

## Usage

``` r
read.fs.volume.nrrd(
  filepath,
  flatten = FALSE,
  with_header = FALSE,
  drop_empty_dims = FALSE
)
```

## Arguments

- filepath:

  character string, path to the file in NRRD format.

- flatten:

  logical, whether to return a numeric vector instead of a
  multidimensional array.

- with_header:

  logical, whether to return an `fs.volume` instance (a named list with
  the entries `data` and `header`) instead of the data array. The header
  contains the geometry of the volume in the `vox2ras_matrix` entry, see
  [`read.nrrd.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nrrd.header.md).

- drop_empty_dims:

  logical, whether to drop dimensions of length 1 from the returned
  data, e.g. the frame axis of a 3D volume that was stored as a 4D array
  with one volume. Note that NRRD files state their dimensionality
  explicitly, so unlike the MGH or NIfTI readers this one only drops
  dimensions that the file actually has.

## Value

a multidimensional array of the dimensions stated in the file header, a
numeric vector if `flatten` is `TRUE`, or an `fs.volume` instance if
`with_header` is `TRUE`. The data type of the array follows the file:
integer for the integer types up to 32 bit, double for the floating
point types and for the 64 bit integer types (which have no R
equivalent, values above 2^53 lose precision, and a warning is raised in
that case).

## Note

The data are returned exactly as they are stored in the file: the reader
does not reorient, rescale or otherwise change them. Use the
`vox2ras_matrix` entry of the header to map voxel indices to RAS+
coordinates, which is the space in which FreeSurfer, NIfTI and the rest
of this package work. NRRD files that state the LPS space (which is what
the ITK tools write) are handled: the matrix is converted to RAS. For a
volume whose file states no space information at all, no matrix is
available (the entry is NULL), since any matrix would be a guess.

Diffusion MRI data in NRRD format carry their b-value and their gradient
directions in custom header fields (the convention of the `teem` tools,
DTI-TK and 3D Slicer). They are parsed into the `dwi` entry of the
header by
[`read.nrrd.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nrrd.header.md).
The gradients are given in the image frame, and the `measurement_frame`
entry of the `dwi` list is the rotation that maps them into the frame in
which the measurement was performed; a caller that needs the gradients
in that frame has to apply it (multiply the vector by the matrix). The
values can be handed to
[`read.dti.gradients`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.gradients.md)
together with one b-value per volume, see the examples.

## Examples

``` r
nrrd_file <- system.file("extdata", "nrrd", "vol_u8_raw.nrrd",
  package = "freesurferformats", mustWork = TRUE
)
vol <- read.fs.volume.nrrd(nrrd_file)
dim(vol)
#> [1] 4 3 2

# Read the geometry as well:
volh <- read.fs.volume.nrrd(nrrd_file, with_header = TRUE)
volh$header$vox2ras_matrix
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1

if (FALSE) { # \dontrun{
# Read a DWI volume and use its gradient table:
hdr <- read.nrrd.header("dwi.nrrd")
gradients <- read.dti.gradients(hdr$dwi$bvec, rep(hdr$dwi$b_value, hdr$dwi$num_gradients))
} # }
```
