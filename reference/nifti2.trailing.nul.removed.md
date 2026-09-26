# Remove trailing NUL bytes from a raw vector.

Remove the NUL bytes at the end of a raw vector, but keep any NUL bytes
before them. This is needed for the payload of NIFTI v2 header
extensions, which is padded with NUL bytes to a multiple of 16 bytes,
but can in theory contain NUL bytes as part of the payload.

## Usage

``` r
nifti2.trailing.nul.removed(rawdata)
```

## Arguments

- rawdata:

  raw vector or integer vector with values in range 0 to 255.

## Value

raw vector, the input without trailing NUL bytes.
