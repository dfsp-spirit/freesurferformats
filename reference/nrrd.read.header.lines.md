# Read the ASCII header of a NRRD file.

Reads the header bytes of a file in NRRD format, up to and including the
blank line that terminates the header, and returns the header lines
together with the byte offset at which the data starts. The whole header
is ASCII text, so it is safe to read it as a string; the binary data
behind it is never touched here.

## Usage

``` r
nrrd.read.header.lines(filepath, max_header_bytes = 10L * 1024L * 1024L)
```

## Arguments

- filepath:

  character string, path to the file.

- max_header_bytes:

  integer, the maximum number of bytes to read while looking for the
  blank line that ends the header. NRRD headers are small (a few KB even
  for large DWI gradient tables), this limit only exists to keep a
  malformed file from producing an endless read.

## Value

named list with entries `lines` (character vector, the header lines
without the terminating blank line), `data_offset` (numeric, the byte
offset at which the data starts, counted from the beginning of the file,
or NA for a gzip-compressed file, in which the data cannot be seeked
to), `gzipped_file` (logical, whether the whole file is gzip-compressed)
and `header_bytes` (integer, the number of bytes the header occupies).
