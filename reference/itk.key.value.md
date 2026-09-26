# Read the value of a key of an ITK transform file.

ITK text transforms store their content as 'key: value' lines, e.g.
'Transform: AffineTransform_float_3_3'. This helper returns the value of
such an entry.

## Usage

``` r
itk.key.value(lines, key)
```

## Arguments

- lines:

  character vector, the key/value lines of the file.

- key:

  character string, the name of the entry, e.g. 'Transform'.

## Value

`NULL` if the entry does not exist, its value as a character string
otherwise.
