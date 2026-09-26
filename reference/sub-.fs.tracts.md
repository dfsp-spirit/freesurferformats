# Subset an fs.tracts instance.

Subset an fs.tracts instance.

## Usage

``` r
# S3 method for class 'fs.tracts'
x[i]
```

## Arguments

- x:

  an `fs.tracts` instance.

- i:

  index vector (integer, numeric or logical), as usual in R.

## Value

a new `fs.tracts` instance containing the selected tracts.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck");
first_ten <- tck$tracks[1:10];
} # }
```
