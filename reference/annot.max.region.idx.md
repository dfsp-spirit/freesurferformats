# Get max region index of an fs.annot instance.

Get max region index of an fs.annot instance.

## Usage

``` r
annot.max.region.idx(annot)
```

## Arguments

- annot:

  fs.annot instance

## Value

integer, the max region index. They typically start with 0 and are
consecutive, but this is not enforced or checked in any way.

## Note

This is a helper function to be used with `annot.unique`, see the
example there.
