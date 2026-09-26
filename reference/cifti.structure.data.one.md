# Extract the data of one brain structure (internal).

Extract the data of one brain structure (internal).

## Usage

``` r
cifti.structure.data.one(cii, map, brainordinates, data, structure)
```

## Arguments

- cii:

  an `fs.cifti` instance, see
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).

- map:

  the indices map of the brainordinate dimension, see
  [`cifti.map.for.dim`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.map.for.dim.md).

- brainordinates:

  the brainordinate table, see
  [`cifti.grayordinates`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md).

- data:

  the data array with the brainordinate dimension first, see
  [`cifti.dim.to.front`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.to.front.md).

- structure:

  character string, the canonical name of the structure.

## Value

a named list, see
[`cifti.structure.data`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md).
