# Stop unless surf is an fs.surface

Stop unless surf is an fs.surface

## Usage

``` r
assert.surface(surface, param_name = "surface")
```

## Arguments

- surface:

  fs.surface instance or anything else

- param_name:

  character string, used in stop message to identify the parameter.

## Value

Called for the side effect of stopping if surface is not an fs.surface
instance.
