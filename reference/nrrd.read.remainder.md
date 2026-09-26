# Read the remaining bytes of a connection.

Reads everything from the current position to the end of the file, in
chunks, to keep the peak memory of the intermediate buffers bounded.
This is used for the bzip2 encoding, which R cannot decompress as a
stream.

## Usage

``` r
nrrd.read.remainder(con)
```

## Arguments

- con:

  a connection opened in binary read mode.

## Value

raw vector.
