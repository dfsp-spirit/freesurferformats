# Determine the header and data file of an ANALYZE 7.5 or NIFTI v1 pair file.

The two formats ANALYZE 7.5 and NIFTI v1 (in its two-file variant) store
the image in a header file (`<base>.hdr`) and a data file
(`<base>.img`). This function computes the two file names from any of
them, so that the user can pass any of the possible spellings.

## Usage

``` r
analyze.pair.files(filepath, require_header = FALSE)
```

## Arguments

- filepath:

  character string, the path to the header file, to the data file, or to
  the base name (with or without a `.hdr`/`.img`/`.gz` suffix).

- require_header:

  logical, whether to stop with an error if the header file does not
  exist. If `FALSE`, the computed file names are returned even if the
  files are missing.

## Value

named list with the entries `header` and `image`, the full paths to the
header and the data file, the logical entries `header_exists` and
`image_exists`, and the entries `mat` (the path of the MATLAB sidecar
file that SPM and FreeSurfer write next to the image file, see
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md))
and `mat_exists`, which report whether such a file is present.

## Note

Compression is handled the way the other implementations of the format
handle it: the suffix `.gz` (and `.hdr`/`.img`) is stripped from the
file name to get the base name, and the two file names are constructed
from it. A name like `vol.hdr.gz` therefore describes the pair
`vol.hdr.gz` and `vol.img.gz`, and `vol.nii.gz` would be the pair
`vol.nii.hdr.gz` and `vol.nii.img.gz`. Note that many tools only read
uncompressed pairs, so compressed pairs should only be written if the
software that reads them supports this.
