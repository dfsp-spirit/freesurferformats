# Read a numerical entry of an ITK transform file.

Read a numerical entry of an ITK transform file.

## Usage

``` r
itk.numeric.value(lines, key, filepath)
```

## Arguments

- lines:

  character vector, the key/value lines of the file.

- key:

  character string, the name of the entry, e.g. 'Parameters'.

- filepath:

  character string, the path of the file, used in error messages only.

## Value

numerical vector, the values of the entry. It is an error if the entry
is missing or holds no numbers.
