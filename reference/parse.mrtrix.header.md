# Parse the key-value pairs of an MRtrix TCK/TSF header.

Splits each header line at its first colon. Values may contain colons
(e.g., in `command_history` entries), they are preserved. Lines without
a colon are ignored, as the MRtrix reference implementation does. Keys
that occur more than once are collected into a vector.

## Usage

``` r
parse.mrtrix.header(lines, filepath = "")
```

## Arguments

- lines:

  character vector, the header lines, including the first line and the
  terminating 'END' line.

- filepath:

  character string, the path of the file, used in error messages only.

## Value

named list, the parsed header.
