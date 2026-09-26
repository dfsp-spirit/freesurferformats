# Build the text header of an MRtrix streamlines file.

MRtrix streamlines files (TCK for the tracks, TSF for per-point values
along the tracks) share one header format, they only differ in the
identifier line and in the meaning of the payload. The header is ASCII
text, and its length is stored within the header itself as the offset at
which the binary payload starts.

## Usage

``` r
build.mrtrix.header(header_id, entries, datatype, count, offset)
```

## Arguments

- header_id:

  character string, the file type identifier, one of 'mrtrix tracks'
  (TCK) or 'mrtrix track scalars' (TSF).

- entries:

  named list of additional header entries.

- datatype:

  character string, the datatype.

- count:

  integer, the number of streamlines.

- offset:

  numeric, the byte offset at which the payload starts.

## Value

character string, the header including the terminating newline.
