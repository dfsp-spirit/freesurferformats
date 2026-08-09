# Read morphometry data from plain text file

Read morphometry data from plain text file

## Usage

``` r
read.fs.morph.txt(filepath)
```

## Arguments

- filepath:

  path to a file in plain text format. Such a file contains, on each
  line, a single float value. This very simply and limited *format* is
  used by the LGI tool by Lyu et al., and easy to generate in shell
  scripts.

## Value

numeric vector, the curv data
