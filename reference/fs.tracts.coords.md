# Get the concatenated coordinates of fs.tracts instances.

Returns all tract coordinates as a single N x 3 matrix, with the tracts
concatenated along the rows. This is the compact representation used
internally, and the fastest way to access all coordinates, e.g., for
plotting or for computing a bounding box.

## Usage

``` r
fs.tracts.coords(tracts)
```

## Arguments

- tracts:

  an `fs.tracts` instance, as returned in the `tracks` entry of
  `read.dti.tck` or `read.dti.trk`.

## Value

numeric matrix with 3 columns and one row per point of all tracts.

## Examples

``` r
if (FALSE) { # \dontrun{
tck <- read.dti.tck("brain.tck");
coords <- fs.tracts.coords(tck$tracks);
bbox <- apply(coords, 2, range);
} # }
```
