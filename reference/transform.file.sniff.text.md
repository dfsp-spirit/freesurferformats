# Extract the text of a file for format sniffing.

Read the beginning of a file and return the printable characters it
contains, so that the file can be identified by markers in its content
without failing on binary data.

## Usage

``` r
# S3 method for class 'file.sniff.text'
transform(filepath, num_bytes = 256L)
```

## Arguments

- filepath:

  character string, the path to the file.

- num_bytes:

  integer, the number of bytes to inspect.

## Value

character string, the printable characters of the beginning of the file.
