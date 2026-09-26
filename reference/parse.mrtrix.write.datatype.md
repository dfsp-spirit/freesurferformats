# Parse and validate the datatype of an MRtrix streamlines file.

The TCK and TSF formats store float values of 32 or 64 bit, in either
byte order.

## Usage

``` r
parse.mrtrix.write.datatype(datatype)
```

## Arguments

- datatype:

  character string, the datatype.

## Value

named list with entries `dsize` (bytes per value) and `endian` ('little'
or 'big').
