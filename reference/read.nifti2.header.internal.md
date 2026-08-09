# Read NIFTI v2 header from file.

Read NIFTI v2 header from file.

## Usage

``` r
read.nifti2.header.internal(filepath, little_endian = TRUE)
```

## Arguments

- filepath:

  path to a NIFTI v2 file.

- little_endian:

  internal logical, leave this alone. Endianness will be figured out
  automatically, messing with this parameter only hurts.

## Value

named list with NIFTI 2 header fields.

## Note

See https://nifti.nimh.nih.gov/pub/dist/data/nifti2/ for test data.
Thanks to Anderson Winkler for his post at
https://brainder.org/2015/04/03/the-nifti-2-file-format/.
