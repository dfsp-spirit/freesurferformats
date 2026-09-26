# Collect the vertices of each region of an annotation.

Collect the vertices of each region of an annotation.

## Usage

``` r
cifti.annot.parcels(annot, structure_name, default_label_name = "unknown")
```

## Arguments

- annot:

  an `fs.annot` instance, see
  [`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md).

- structure_name:

  character string, the brain structure of the annotation, used in error
  messages.

- default_label_name:

  character string, the name for vertices without a region name.

## Value

a named list of integer vectors, one entry per region that contains at
least one vertex: the 0-based vertex indices of the region, in ascending
order. The entries are ordered like the label table of the annotation
(the atlas order).
