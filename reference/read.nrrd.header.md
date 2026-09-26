# Read the header of a file in NRRD format.

Reads and parses the ASCII header of a NRRD file without touching the
volume data. This is cheap, and it can be used to inspect a file, or to
check its geometry, before deciding whether to read the data.

## Usage

``` r
read.nrrd.header(filepath)
```

## Arguments

- filepath:

  character string, path to the NRRD file (`.nrrd` or `.nhdr`).

## Value

named list, the parsed header. The fields of the file are available
under their normalized names (e.g. `space_directions` for the field
'space directions'), and fields that this function does not interpret
are collected in the `other_fields` entry. The derived entries are
`vox2ras_matrix` (the 4x4 matrix that maps 0-based voxel indices to RAS+
coordinates, or NULL when the file states no geometry), `vox2ras_source`
(how the matrix was derived: 'space directions' or 'spacings'),
`data_files` (the resolved paths of the data files for a detached
header, or NULL), `data_offset` (the byte offset of the data in the
header file), `gzipped_file` (whether the whole file is
gzip-compressed), `header_size` (the size of the header in bytes) and
`dwi` (the parsed diffusion metadata, or NULL, see
[`read.fs.volume.nrrd`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.nrrd.md)).

## Examples

``` r
nrrd_file <- system.file("extdata", "nrrd", "vol_u8_raw.nrrd",
  package = "freesurferformats", mustWork = TRUE
)
hdr <- read.nrrd.header(nrrd_file)
hdr$sizes
#> [1] 4 3 2
hdr$vox2ras_matrix
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
```
