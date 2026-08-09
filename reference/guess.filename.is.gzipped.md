# Guess whether a file is gzipped.

Guess whether a file is gzipped, based on the file extension.

## Usage

``` r
guess.filename.is.gzipped(filepath, gz_extensions = c(".gz", ".mgz"))
```

## Arguments

- filepath:

  string. Path to a file.

- gz_extensions:

  list of strings. A list of suffixes that is considered indicative for
  the file being gzipped. Defaults to c(".gz", ".mgz"). Case does not
  matter.

## Value

logical, whether this function thinks the file is gzipped.
