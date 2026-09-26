# Create a CIFTI-2 axis for named maps (internal helper).

Create a CIFTI-2 axis for named maps (internal helper).

## Usage

``` r
cifti.axis.named.maps(names, metadata, label_tables, type)
```

## Arguments

- names:

  character vector, the map names, or `NULL` for maps without a name.

- metadata:

  list of metadata lists, or `NULL`.

- label_tables:

  list of label tables, or `NULL`.

- type:

  character string, 'CIFTI_INDEX_TYPE_SCALARS' or
  'CIFTI_INDEX_TYPE_LABELS'.

## Value

a named list with the entries 'type' and 'named_maps'.
