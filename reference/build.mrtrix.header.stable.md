# Determine the length of an MRtrix streamlines header.

The length of the header determines the data offset that is stored
*inside* the header, so the two have to be reconciled: the offset is
written with a number of digits that depends on its own value, which can
change the length of the header. Starting from an offset guess and
recomputing until it is stable always terminates, since the length only
ever grows with the number of digits and that number is bounded.

## Usage

``` r
build.mrtrix.header.stable(header_id, entries, datatype, count)
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

## Value

named list with entries `text` (the header) and `offset`.
