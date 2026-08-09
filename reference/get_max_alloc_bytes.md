# Get the configured maximum allocation size in bytes.

Returns the maximum number of bytes that the package is allowed to
allocate when reading binary data payloads. The limit is resolved in
this order: (1) environment variable
`FREESURFERFORMATS_MAX_ALLOC_BYTES`, (2) R option
`freesurferformats.max_alloc_bytes`, (3) the hard-coded default of 2 GB.
Set the limit to `Inf` to disable the check.

## Usage

``` r
get_max_alloc_bytes()
```

## Value

a single numeric value, the maximum allocation size in bytes.
