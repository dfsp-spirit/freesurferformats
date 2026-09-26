# Parse the 'file' entry of an MRtrix TCK/TSF header.

The entry has the form 'file: . OFFSET', where the file name part must
be a single dot for single-file TCK/TSF files and the offset gives the
byte position at which the binary payload starts.

## Usage

``` r
parse.mrtrix.file.entry(header, filepath = "")
```

## Arguments

- header:

  named list, the parsed header.

- filepath:

  character string, the path of the file, used in messages only.

## Value

named list with entries `filename_part` (character) and `offset`
(numeric).
