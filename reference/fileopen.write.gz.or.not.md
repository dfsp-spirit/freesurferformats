# Open a connection for writing, with gzip support based on the file name.

Open a connection for writing, with gzip support based on the file name.

## Usage

``` r
fileopen.write.gz.or.not(filepath)
```

## Arguments

- filepath:

  character string, the path of the file to write.

## Value

a connection, either a `gzfile` or a plain file connection.
