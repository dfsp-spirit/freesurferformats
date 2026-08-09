# Create new bvsmp instance encoding morph data for Brainvoyager.

Create new bvsmp instance encoding morph data for Brainvoyager.

## Usage

``` r
bvsmp(morph_data)
```

## Arguments

- morph_data:

  numeric vector, the morphometry data to store in the bvsmp instance
  (one value per mesh vertex).

## Value

bvsmp instance, can be used to write Brainvoyager SMP format morphometry
files using
[`write.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/write.smp.brainvoyager.md).
Modify as needed before writing.

## Examples

``` r
morph_data <- rnorm(100L, 3.0, 1.0)
mybvsmp <- bvsmp(morph_data)
mybvsmp$smp_version
#> [1] 3
```
