# Write an fs.volume instance to a file in MGH, MGZ or NIFTI v1 format.

Write brain volume data to a file. The format is determined from the
file extension of `filepath`.

## Usage

``` r
write.fs.volume(filepath, fs_vol)
```

## Arguments

- filepath:

  string. Full path to the output file. The file extension determines
  the format: '.mgh' or '.mgz' for FreeSurfer MGH/MGZ format, '.nii' or
  '.nii.gz' for NIFTI v1 format, and '.hdr', '.img', '.hdr.gz' or
  '.img.gz' for a NIFTI v1 pair, i.e. a header file with a separate data
  file. A pair is written instead of a plain ANALYZE 7.5 file because
  ANALYZE cannot store the geometry of the volume, see
  [`write.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/write.analyze.md)
  if you need real ANALYZE output.

- fs_vol:

  an `fs.volume` instance, as returned by
  [`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
  with parameter `with_header=TRUE`.

## Note

When writing NIFTI files, this function uses
[`nii1header.for.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/nii1header.for.mgh.md)
to compute a NIFTI v1 header from the MGH header information.

A file name that is one of the standard CIFTI-2 names (e.g.
`.dscalar.nii`) is an error: such a file has to contain the CIFTI XML
metadata, so a NIFTI file with that name is refused by this package and
misread by other software. Use
[`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md)
for CIFTI-2 files.

## Examples

``` r
if (FALSE) { # \dontrun{
mgh_file <- system.file("extdata", "brain.mgz",
  package = "freesurferformats",
  mustWork = TRUE
)
fs_vol <- read.fs.volume(mgh_file, with_header = TRUE)
write.fs.volume(tempfile(fileext = ".mgz"), fs_vol)
write.fs.volume(tempfile(fileext = ".nii.gz"), fs_vol)
write.fs.volume(tempfile(fileext = ".hdr"), fs_vol)
} # }
```
