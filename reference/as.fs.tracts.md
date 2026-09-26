# Convert a collection of tracts to an fs.tracts instance.

Accepts an `fs.tracts` instance, a list of numeric matrices with 3
columns, or a single such matrix (which is then treated as one tract),
and returns an `fs.tracts` instance. This is what the writers use to
accept several input types.

## Usage

``` r
as.fs.tracts(tracts, kind = "tck")
```

## Arguments

- tracts:

  the input, see the description.

- kind:

  character string, 'tck' or 'trk', used for the returned instance.

## Value

an `fs.tracts` instance.

## Examples

``` r
tracts <- as.fs.tracts(list(matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE)));
length(tracts);
#> [1] 1
```
