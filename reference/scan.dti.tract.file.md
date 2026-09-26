# Scan a tract file without holding its data in memory.

Streams through the tract file and either counts the tracts, or computes
the bounding box of all their points, or both. Nothing but the current
chunk is ever held in memory, so this works on files of any size.

## Usage

``` r
scan.dti.tract.file(filepath, want = c("count", "bbox"), chunk_values = 4e+06)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

- want:

  character vector, any combination of 'count' and 'bbox'.

- chunk_values:

  integer, number of payload values to read per chunk.

## Value

named list with the entries that were requested: `count` (integer, the
number of tracts) and `bbox` (numeric vector of length 6,
`c(xmin, xmax, ymin, ymax, zmin, zmax)`), or `NULL` for `bbox` if no
point was found.
