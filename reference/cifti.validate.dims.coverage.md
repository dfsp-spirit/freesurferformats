# Check that all matrix dimensions are described exactly once.

Check that all matrix dimensions are described exactly once.

## Usage

``` r
cifti.validate.dims.coverage(indices_maps, dim_sizes, filepath = "")
```

## Arguments

- indices_maps:

  list of parsed indices maps.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

## Value

`NULL`, invisibly. Stops if a dimension is described twice or not at
all.
