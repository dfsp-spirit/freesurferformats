# Read the header of a file in MRtrix 'TCK' or 'TSF' format.

Reads only the ASCII header of the file, without touching the binary
payload. This is cheap even for huge tractograms and can be used to
inspect a file before deciding whether to read its tracks.

## Usage

``` r
read.dti.tck.header(filepath)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

## Value

named list, the parsed header. The entries of the 'derived' entry
describe the payload (data offset, datatype, endianness, and whether the
file is gzip-compressed).

## Examples

``` r
if (FALSE) { # \dontrun{
hdr <- read.dti.tck.header("brain.tck");
hdr$count;              # number of streamlines stated in the header
hdr$derived$gzipped;    # TRUE for a .tck.gz file
} # }
```
