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
  '.nii.gz' for NIFTI v1 format.

- fs_vol:

  an `fs.volume` instance, as returned by
  [`read.fs.volume`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
  with parameter `with_header=TRUE`.

## Note

When writing NIFTI files, this function uses
[`nii1header.for.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/nii1header.for.mgh.md)
to compute a NIFTI v1 header from the MGH header information.

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
} # }
```
