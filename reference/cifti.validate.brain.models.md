# Validate the brain model entries of a CIFTI mapping.

Validate the brain model entries of a CIFTI mapping.

## Usage

``` r
cifti.validate.brain.models(map, dim_size, filepath = "")
```

## Arguments

- map:

  a parsed indices map, see
  [`cifti.parse.indices.map`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parse.indices.map.md).

- filepath:

  character string, the file path, used in error messages.

## Value

`NULL`, invisibly. Stops with a descriptive error if an entry is
inconsistent.
