# Read the 4 magic bytes of an ANALYZE 7.5 or NIFTI v1 header.

Read the 4 magic bytes of an ANALYZE 7.5 or NIFTI v1 header.

## Usage

``` r
analyze.read.magic(filepath)
```

## Arguments

- filepath:

  character string, the path to the header file of a pair.

## Value

character string, the 4 bytes at offset 344 of the file, interpreted as
a string. This is the NIFTI v1 `magic` field, which is empty (`''`) for
ANALYZE 7.5 files (where the same bytes are the `smin` field), `'ni1'`
for a NIFTI v1 pair file and `'n+1'` for a single file NIFTI v1 file.
