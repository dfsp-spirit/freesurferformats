# Read Brainvoyager vertex-wise statistical surface data from SMP file.

Read Brainvoyager vertex-wise statistical surface data from SMP file.

## Usage

``` r
read.fs.morph.bvsmp(filepath, map_index = 1L)
```

## Arguments

- filepath:

  character string, path to file in Brainvoyager SMP file format.
  Alternatively, a 'bvsmp' instance read with
  [`read.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/read.smp.brainvoyager.md).

- map_index:

  positive integer or character string, the surface value map to load
  (an SMP file can contain several values per vertex, i.e., several
  surface maps). If an integer, interpreted as the index of the map. If
  a character string, as the name of the map.

## Value

numeric vector, the values from the respective map.
