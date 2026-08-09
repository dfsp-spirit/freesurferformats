# Read mesh in BYU format.

The BYU or Brigham Young University format is an old ASCII mesh format
that is based on fixed character positions in lines (as opposed to
whitespace-separated elements). I consider it a bit counter-intuitive.

## Usage

``` r
read.fs.surface.byu(filepath, part = 1L)
```

## Arguments

- filepath:

  full path of the file in BYU format.

- part:

  positive integer, the index of the mesh that should be loaded from the
  file. Only relevant if the file contains more than one mesh.

## Value

an `fs.surface` instance, aka a mesh

## References

See http://www.eg-models.de/formats/Format_Byu.html for a format
description.
