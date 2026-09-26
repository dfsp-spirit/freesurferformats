# Accept the input forms of a set of annotations.

Accept the input forms of a set of annotations.

## Usage

``` r
cifti.annot.list(annots, structure = NULL)
```

## Arguments

- annots:

  an `fs.annot` instance or a named list of them.

- structure:

  character string or `NULL`, the brain structure of a single
  annotation.

## Value

a named list of `fs.annot` instances, named by canonical brain structure
name.
