# Scan an MRtrix TCK or TSF file.

Scan an MRtrix TCK or TSF file.

## Usage

``` r
scan.mrtrix.file(filepath, want, chunk_values = 4e+06)
```

## Arguments

- filepath:

  character string, path to the file.

- want:

  character vector, the values to compute, see `scan.dti.tract.file`.

- chunk_values:

  integer, number of payload values to read per chunk.

## Value

named list with entries `count` and `bbox`.
