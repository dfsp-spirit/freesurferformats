# Convert an fs.tracts instance to a plain list of tracts.

Converts to the classic representation as a `list` with one entry per
tract. This is convenient for interacting with code that expects a plain
list, but note that it materializes one R object per tract and is
therefore much more expensive in both memory and time. Prefer `[[` for
accessing individual tracts.

## Usage

``` r
# S3 method for class 'fs.tracts'
as.list(x, ...)
```

## Arguments

- x:

  an `fs.tracts` instance.

- ...:

  ignored.

## Value

list with one entry per tract.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck");
tracts_list <- as.list(tck$tracks);
} # }
```
