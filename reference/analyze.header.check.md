# Check whether an ANALYZE 7.5 header is suitable for writing.

Check whether an ANALYZE 7.5 header is suitable for writing.

## Usage

``` r
analyze.header.check(analyzeheader)
```

## Arguments

- analyzeheader:

  named list, an ANALYZE 7.5 header as returned by
  [`analyzeheader.template`](https://dfsp-spirit.github.io/freesurferformats/reference/analyzeheader.template.md).

## Value

logical, whether the header passed the checks. Fields that cannot be
written as requested are reported with a `message`.

## Note

The checks are in no way meant to be exhaustive. They only verify that
the fields that are written to the file have a length that fits into
their fixed size slots in the 348 byte header, since writing a longer
value would shift all fields behind it and produce a corrupt file.
