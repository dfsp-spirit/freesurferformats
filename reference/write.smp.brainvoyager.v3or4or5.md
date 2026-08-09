# Write a brainvoyager v3, v4 or v5 SMP file.

Write a brainvoyager v3, v4 or v5 SMP file.

## Usage

``` r
write.smp.brainvoyager.v3or4or5(filepath, bvsmp, smp_version)
```

## Arguments

- filepath:

  character string, the output file

- bvsmp:

  bvsmp instance, a named list as returned by
  [`read.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/read.smp.brainvoyager.md).

- smp_version:

  integer, the SMP file format version to use when writing. Versions 2
  to 5 are supported, but only versions 2 and 3 have been tested
  properly. Please report any problems you encounter. When converting
  between file versions (e.g., loading a v2 file and saving the result
  as a v5 file), some required fields may be missing, and for those
  without a default value according to the official spec, you will have
  to manually add the value you want in the bvsmp object before writing.

## Note

Called by
[`write.smp.brainvoyager`](https://dfsp-spirit.github.io/freesurferformats/reference/write.smp.brainvoyager.md).
