# Normalize a CIFTI brain structure name.

CIFTI brain structure names are written in several spellings by
different software: Connectome Workbench uses
`CIFTI_STRUCTURE_CORTEX_LEFT`, nibabel also accepts `CortexLeft`, and
this package uses the aliases `lh` and `rh` (and also `left`/`right`)
for the cortical surfaces. This function normalizes all of them to the
canonical `CIFTI_STRUCTURE_*` spelling.

## Usage

``` r
cifti.structure.canonical(brain_structure)
```

## Arguments

- brain_structure:

  character string, a brain structure name in any of the supported
  spellings.

## Value

character string, the canonical structure name (e.g.
'CIFTI_STRUCTURE_CORTEX_LEFT'), or `NA_character_` if the input is `NA`.
