# Read Brainvoyager statistical surface results from SMP file.

Read Brainvoyager statistical surface results from SMP file.

## Usage

``` r
read.smp.brainvoyager(filepath)
```

## Arguments

- filepath:

  character string, path to file in Brainvoyager SMP file format

## Value

named list of file contents

## Note

Currently only SMP file versions 1 to 5 are supported, as these are the
only ones for which a spec is available. The version is encoded in the
file header.

## References

see
https://helpdesk.brainvoyager.com/brainvoyager/automation-development/84-file-formats/40-the-format-of-smp-files
for the spec

## Examples

``` r
if (FALSE) { # \dontrun{
# Surface mesh, requires BV demo dataset from website:
sf <- read.fs.surface.bvsrf("~/data/BrainTutorData/CG_LHRH_D65534.srf")
# Surface map of cortical thickness. Needs to be created in BV.
smp_file <- "~/data/BrainTutorData/CG_LHRH_D65534_Thickness.smp"
smp <- read.smp.brainvoyager(smp_file)
smp_data <- read.fs.morph.bvsmp(smp)
# could also pass smp_file.
fsbrain::vis.fs.surface(sf, per_vertex_data = smp_data)
} # }
```
