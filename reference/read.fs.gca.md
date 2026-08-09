# Read FreeSurfer GCA file.

Read FreeSurfer GCA file.

## Usage

``` r
read.fs.gca(filepath)
```

## Arguments

- filepath:

  character string, path to a file in binary GCA format. Stores array of
  Gaussian classifiers for probabilistic atlas.

## Value

named list, the file fields. The GCA data is in the data field.

## Author

This function is based on Matlab code by Bruce Fischl, published under
the FreeSurfer Open Source License available at
<https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense>.
The R version was written by Tim Schaefer.

## Examples

``` r
if (FALSE) { # \dontrun{
gca_file <- file.path(Sys.getenv("FREESURFER_HOME"), "average", "face.gca")
gca <- read.fs.gca(gca_file)
} # }
```
