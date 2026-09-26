# Validate and describe the datatype entry of an MRtrix TCK/TSF header.

Validate and describe the datatype entry of an MRtrix TCK/TSF header.

## Usage

``` r
parse.mrtrix.datatype(header, filepath = "")
```

## Arguments

- header:

  named list, the parsed header.

- filepath:

  character string, the path of the file, used in messages only.

## Value

named list with entries `dsize` (bytes per value) and `endian` ('little'
or 'big').
