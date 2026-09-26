# Resolve a user-supplied structure specifier to a canonical name.

Accept the structure spellings the user-facing readers support: a
structure name in any of the supported spellings, or the index of a
structure in the file (deprecated, but part of the documented interface
of the readers).

## Usage

``` r
cifti.structure.from.specifier(cii, brain_structure)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- brain_structure:

  character string or integer, the specifier.

## Value

character string, the canonical structure name.
