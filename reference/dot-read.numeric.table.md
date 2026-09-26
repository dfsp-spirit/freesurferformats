# Read a whitespace-separated numeric table from a text file.

Low-level parser shared by all gradient table readers. Values may be
separated by spaces, tabs or commas, and both blank lines and lines
starting with `#` are ignored. All remaining lines must contain the same
number of values.

## Usage

``` r
.read.numeric.table(filepath, allow_count_header = FALSE)
```

## Arguments

- filepath:

  character string, path to the file. Gzip-compressed files are detected
  by their magic bytes, not by the file name.

- allow_count_header:

  logical, whether a first line that consists of a single integer may be
  interpreted as a count header and dropped. Only the MRtrix gradient
  table reader enables this, because the historical `.grad`/`.b` files
  may start with the number of volumes.

## Value

a numeric matrix, one row per input line.
