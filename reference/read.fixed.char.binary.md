# Read fixed length char, possibly containing embedded zeroes, from binary file.

Read fixed length char, possibly containing embedded zeroes, from binary
file.

## Usage

``` r
read.fixed.char.binary(filehandle, n, to = "UTF-8")
```

## Arguments

- filehandle:

  connection to read.

- n:

  the number of bytes to read.

- to:

  the target character encoding.

## Value

the string in the target encoding, with the embedded zeroes removed.

## Author

The original version was written by Brandon Whitcher and Volker Schmid.
See the source for the full legal info. The coding style was adapted to
freesurferformats and the docs were added by Tim Schäfer.
