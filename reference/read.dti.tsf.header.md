# Read the header of a file in MRtrix 'TSF' format.

Reads only the ASCII header of the file, see
[`read.dti.tck.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.header.md).

## Usage

``` r
read.dti.tsf.header(filepath)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

## Value

named list, the parsed header.

## Examples

``` r
if (FALSE) { # \dontrun{
hdr <- read.dti.tsf.header("brain.tsf");
} # }
```
