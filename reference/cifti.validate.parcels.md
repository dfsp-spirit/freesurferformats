# Validate the parcels of a CIFTI mapping.

Validate the parcels of a CIFTI mapping.

## Usage

``` r
cifti.validate.parcels(map, filepath = "")
```

## Arguments

- map:

  a parsed indices map, see
  [`cifti.parse.indices.map`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parse.indices.map.md).

- filepath:

  character string, the file path, used in error messages.

## Value

`NULL`, invisibly. Stops with a descriptive error if a parcel refers to
vertices outside of a declared surface.
