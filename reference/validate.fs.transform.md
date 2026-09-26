# Check whether an object is a valid fs.transform instance.

This checks the invariants that every `fs.transform` must fulfill: it is
a named list with a finite 4x4 numerical matrix, a known or unknown
coordinate space for both sides, a voxel base that matches those spaces,
and volume descriptors that only contain supported entries. It is called
automatically by the constructor, so that readers, writers and user code
can rely on the fields.

## Usage

``` r
validate.fs.transform(tf)
```

## Arguments

- tf:

  the object to check.

## Value

`TRUE` if `tf` is a valid fs.transform instance, and the function
`stop`s with an explanatory error message otherwise.
