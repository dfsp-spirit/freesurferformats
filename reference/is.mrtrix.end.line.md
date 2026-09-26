# Check whether a header line terminates the MRtrix header.

Check whether a header line terminates the MRtrix header.

## Usage

``` r
is.mrtrix.end.line(line)
```

## Arguments

- line:

  character string, a single header line.

## Value

logical, TRUE if the line reads 'END' (ignoring surrounding whitespace
and a possible DOS line ending).
