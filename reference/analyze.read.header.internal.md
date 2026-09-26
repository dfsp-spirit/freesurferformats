# Read ANALYZE 7.5 header from file.

Read ANALYZE 7.5 header from file.

## Usage

``` r
analyze.read.header.internal(filepath, little_endian = TRUE)
```

## Arguments

- filepath:

  character string, the path to the `.hdr` file. The base name without
  the extension is accepted as well, see
  [`analyze.pair.files`](https://dfsp-spirit.github.io/freesurferformats/reference/analyze.pair.files.md).

- little_endian:

  logical, leave this alone. The endianness is detected automatically,
  and messing with this parameter only makes the detection report a
  wrong endianness for a file of the other one.

## Value

named list, the ANALYZE 7.5 header. See
[`read.analyze.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.analyze.header.md).
