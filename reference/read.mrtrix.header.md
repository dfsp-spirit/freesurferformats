# Read the ASCII header of an MRtrix TCK/TSF file.

Reads lines up to and including the terminating 'END' line. The number
of header lines is not limited, headers of real files contain a variable
number of entries (command history, comments, ROI specifications).

## Usage

``` r
read.mrtrix.header(filepath)
```

## Arguments

- filepath:

  character string, path to the file.

## Value

named list with entries `lines` (the raw header lines), `gzipped`
(logical) and `header` (the parsed key-value pairs).
