# Count the tracts in a DTI tract file.

Counts the tracts in a TRK, TCK or TSF file without reading their
coordinates, so this works on arbitrarily large tractograms with a small
and constant amount of memory. This is the equivalent of
`tckinfo <file> -count` in MRtrix.

## Usage

``` r
dti.track.count(filepath, chunk_values = 4e+06)
```

## Arguments

- filepath:

  character string, path to the `TCK` or `TSF` file to read.
  Gzip-compressed files (i.e., `.tck.gz`) are supported and the
  compression is detected from the file content, not the file name.

- chunk_values:

  integer, the number of payload values to read per chunk. Advanced
  tuning parameter, see
  [`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.md).

## Value

integer, the number of tracts in the file. Note that this can differ
from the `count` entry in the file header, which the MRtrix
documentation explicitly describes as unreliable, and which does not
count empty tracts.

## Examples

``` r
if (FALSE) { # \dontrun{
dti.track.count("brain.tck");
} # }
```
