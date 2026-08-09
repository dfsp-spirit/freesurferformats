# Read DTI tracking per-coord data from file in MRtrix 'TSF' format.

Read DTI tracking per-coord data from file in MRtrix 'TSF' format.

## Usage

``` r
read.dti.tsf(filepath)
```

## Arguments

- filepath:

  character string, path to the `TSF` file to read.

## Value

named list with entries 'header' and 'scalars'. The scalar data are
available in 2 representations: 'merged': a vector of all values
(requires external knowledge on track borders), and 'scalar_list':
organized into a list of vectors. Each vector represents the values for
the points of one track.

## Examples

``` r
if (FALSE) { # \dontrun{
tsff <- "~/simple.tsf"
tsf <- read.dti.tsf(tsff)
} # }
```
