# Determine endianness of TRK file.

Determine endianness of TRK file.

## Usage

``` r
get.dti.trk.endianness(filepath)
```

## Arguments

- filepath:

  character string, path to file in trk format. Gzip-compressed files
  are supported as well (the compression is detected from the file
  content, so a `.trk.gz` file is read like any other TRK file), which
  is convenient since tractograms are large and are regularly stored
  compressed. Note that track files cannot be compressed on the fly for
  other software: the TrackVis tools and MRtrix do not read compressed
  track files.

## Value

endina character string. one of 'little' or 'big'.

## Note

This function checks endiannes via the header size field of the file
header, which must be 1000 for TRK files when read with correct
enianness. It will stop if the file is not in TRK format, i.e., if the
field is not 1000 in any endianness.
