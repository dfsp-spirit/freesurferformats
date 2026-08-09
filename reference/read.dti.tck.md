# Read DTI tracking data from file in MRtrix 'TCK' format.

Read DTI tracking data from file in MRtrix 'TCK' format.

## Usage

``` r
read.dti.tck(filepath)
```

## Arguments

- filepath:

  character string, path to the `TCK` file to read.

## Value

named list with entries 'header' and 'tracks'. The tracks are organized
into a list of matrices. Each n x 3 matrix represents the coordinates
for the n points of one track, the values in each row are the
xyz-coords.

## Examples

``` r
if (FALSE) { # \dontrun{
tckf <- "~/simple.tck"
tck <- read.dti.tck(tckf)
} # }
```
