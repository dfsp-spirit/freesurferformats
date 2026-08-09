# Perform basic sanity checks on NIFTI header data. These are in no way meant to be exhaustive.

Perform basic sanity checks on NIFTI header data. These are in no way
meant to be exhaustive.

## Usage

``` r
nifti.header.check(niiheader, nifti_version = 1L)
```

## Arguments

- niiheader:

  named list, the NIFTI header.

- nifti_version:

  integer, one of 1L or 2L. The NIFTI format version.

## Value

logical, whether the check was okay
