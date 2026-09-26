# Open the payload of an MRtrix TCK/TSF file for reading.

Parses and validates the header and returns everything needed to read
the payload. The caller is responsible for closing `con`.

## Usage

``` r
# S3 method for class 'mrtrix.payload'
open(filepath, max_tracks = Inf)
```

## Arguments

- filepath:

  character string, path to the file.

- max_tracks:

  numeric, the maximum number of tracks that will be read. Used to
  decide whether an up-front allocation check makes sense.

## Value

named list with entries `con`, `header`, `offset`, `dsize`, `endian`,
`values_per_point`, `gzipped`, `is_tck` and `expected_groups`.
