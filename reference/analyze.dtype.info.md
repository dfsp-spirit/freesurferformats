# Compute the R data type and the number of bytes per value for an ANALYZE data type.

Compute the R data type and the number of bytes per value for an ANALYZE
data type.

## Usage

``` r
analyze.dtype.info(analyzeheader, filepath = NULL)
```

## Arguments

- analyzeheader:

  named list, an ANALYZE 7.5 header as returned by
  [`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).

- filepath:

  character string, the path of the file the header was read from. Only
  used for warning and error messages.

## Value

named list with the entries `datatype` and `bitpix` (the data type
fields to use for reading the data, which may differ from the values in
the file if the file contradicts itself), `num_bytes` (the number of
bytes per value) and `r_dtype` (the R type to read the values as).

## Note

ANALYZE files in the wild sometimes have a wrong `bitpix` field, since
the format was used by many tools that did not care about it. The data
type is the reliable field, so if the two fields contradict each other,
the `bitpix` value that belongs to the data type is used, with a
warning.
