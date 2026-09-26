# Get the b-value threshold below which a volume counts as a b=0 volume.

MRtrix3 treats any volume whose b-value is at most this threshold as a
`b=0` volume. See the `BZeroThreshold` configuration entry of MRtrix3,
which defaults to 10 s/mm^2.

## Usage

``` r
.dti.bzero.threshold()
```

## Value

numeric scalar, the threshold in s/mm^2.
