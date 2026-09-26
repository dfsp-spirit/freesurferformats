# Select the data values of the grayordinates of a file.

Select the data values of the grayordinates of a file.

## Usage

``` r
cifti.data.for.grayordinates(data, structures, index_table, what)
```

## Arguments

- data:

  the data, see
  [`write.fs.morph.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md).

- structures:

  character vector, the canonical structure names, in file order.

- index_table:

  the brainordinate table, see
  [`cifti.grayordinates`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md).

- what:

  character string, a description of the data, used in error messages.

## Value

a matrix with one row per grayordinate (in file order) and one column
per map.
