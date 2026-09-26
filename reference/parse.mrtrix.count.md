# Parse the 'count' entry of an MRtrix TCK/TSF header.

The entry is optional: the MRtrix documentation lists only 'file' and
'datatype' as required header keys, and explicitly notes that the value
may not reflect the number of streamlines actually stored, e.g., when a
command was terminated prematurely. The value is therefore used as a
hint only, never to decide how much data to read.

## Usage

``` r
parse.mrtrix.count(header)
```

## Arguments

- header:

  named list, the parsed header.

## Value

numeric, the stored count, or NA if it is absent or not parseable.
