# Read Brainvoyager statistical surface results from v1 or v2 SMP file.

Read Brainvoyager statistical surface results from v1 or v2 SMP file.

## Usage

``` r
read.smp.brainvoyager.v1or2(filepath, version)
```

## Arguments

- filepath:

  character string, path to file in Brainvoyager SMP file format

## Value

named list of file contents

## Note

Do not call this, call `read.smp.brainvoyager` instead, which will
figure out the version and call the appropriate function.
