# Remove NUL bytes from a raw vector.

Remove all NUL bytes from a raw vector. This is needed for the payload
of NIFTI v2 header extensions, which are padded with NUL bytes, and for
the payload of MATLAB files, but the function is not specific to any
format.

## Usage

``` r
nifti2.strip.nul(rawdata)
```

## Arguments

- rawdata:

  raw vector or integer vector with values in range 0 to 255.

## Value

raw vector, the input without any NUL bytes.
