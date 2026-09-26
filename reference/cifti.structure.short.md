# Get the short name of a CIFTI brain structure.

Strip the `CIFTI_STRUCTURE_` prefix from a brain structure name,
normalizing it first, see
[`cifti.structure.canonical`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.canonical.md).

## Usage

``` r
cifti.structure.short(brain_structure)
```

## Arguments

- brain_structure:

  character string, a brain structure name in any of the supported
  spellings.

## Value

character string, the structure name without the prefix (e.g.
'CORTEX_LEFT').
