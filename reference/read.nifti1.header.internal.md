# Read NIFTI v1 header from file (which may contain the FreeSurfer hack).

Read NIFTI v1 header from file (which may contain the FreeSurfer hack).

## Usage

``` r
read.nifti1.header.internal(filepath, little_endian = TRUE)
```

## Arguments

- filepath:

  path to a NIFTI v1 file (single file version), which can contain the
  FreeSurfer hack.

- little_endian:

  internal logical, leave this alone. Endianness will be figured out
  automatically, messing with this parameter only hurts.

## Value

named list with NIFTI 1 header fields.
