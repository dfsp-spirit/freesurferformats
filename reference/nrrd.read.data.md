# Read the data section of a NRRD file.

Reads the values of a volume, from the header file itself or from the
data files it names, and checks that their number matches the dimensions
stated in the header.

## Usage

``` r
nrrd.read.data(filepath, info, type_info)
```

## Arguments

- filepath:

  character string, path to the header file.

- info:

  named list, the parsed header, see `read.nrrd.header`.

- type_info:

  named list, the R data type information, see `nrrd.type.info`.

## Value

vector of values, of length `prod(sizes)`.
