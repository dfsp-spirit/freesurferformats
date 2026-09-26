# Read the header of a VTK legacy file and set up the reader.

Read the header of a VTK legacy file and set up the reader.

## Usage

``` r
vtk.read.header(reader)
```

## Arguments

- reader:

  an environment as returned by
  [`vtk.reader.new`](https://dfsp-spirit.github.io/freesurferformats/reference/vtk.reader.new.md).

## Value

named list with the entries 'version' (character), 'encoding' ('ASCII'
or 'BINARY') and 'dataset' (character, the dataset type).
