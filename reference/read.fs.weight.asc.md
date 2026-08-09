# Read ASCII version of FreeSurfer weight file.

Read ASCII version of FreeSurfer weight file. Called by
[`read.fs.weight`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.weight.md)
if parameter `format` is set to 'asc'.

## Usage

``` r
read.fs.weight.asc(filepath)
```

## Arguments

- filepath:

  string. Full path to the input weight file, must be in ASCII weight
  format.

## Value

the indices and weight data, as a named list. Entries: "vertex_indices":
vector of *n* vertex indices. They are stored zero-based in the file,
but are returned one-based (R-style). "value": double vector of length
*n*, the morphometry data for the vertices. The data can be whatever you
want.
