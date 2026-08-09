# Write a brainvoyager v2 SMP file.

Write a brainvoyager v2 SMP file.

## Usage

``` r
write.smp.brainvoyager.v2(filepath, bvsmp)
```

## Arguments

- filepath:

  character string, the output file

- bvsmp:

  bvsmp instance, a named list as returned by
  [`read.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/read.smp.brainvoyager.md).

## Note

Called by
[`write.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/write.smp.brainvoyager.md).

The map_type and num_lags of the first vertex map will be used for the
top header,i.e., for all maps. The v2 format does not support per-map
settings for these values. Also min_alg, max_lag and cc_overlay are
ignored.
